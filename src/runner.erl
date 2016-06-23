-module(runner).

-export([run/3]).

-behaviour(gen_fsm).
-export([init/1, handle_info/3, handle_event/3, handle_sync_event/4,
         code_change/4, terminate/3, perform_solution/3]).

-ignore_xref([warm_up/2, running/2]).
-export([warm_up/2, running/2]).

-record(state_data, {
          problem  :: string(),
          parsed   :: problem:problem(),
          runtime  :: pos_integer(),
          port     :: port(),
          received :: non_neg_integer(),
          max      :: integer() | undefined,
          max_id   :: non_neg_integer() | undefined,
          performers :: ordsets:ordset(pid())
         }).
-type(state_data() :: #state_data{}).
-type(state_name() :: warmup | running).

%% public API
-spec run(string(), problem:problem(), pos_integer()) -> {ok, pid()}.
run(Problem, Parsed, Time) ->
    gen_fsm:start({local, ?MODULE}, ?MODULE, {Problem, Parsed, Time}, []).

%% gen_fsm API

-spec init({string() | binary(), br_exercise:exercise(), pos_integer()})
          -> {ok, warm_up, state_data()}.
init({Problem, Parsed, Time}) ->
    gen_fsm:send_event_after(1000, {countdown, 5}),
    Make = os:find_executable("make"),
    Port = open_port({spawn_executable, Make}, [{args, [<<"run">>]},
                                                exit_status]),
    {ok, warm_up, #state_data{problem  = Problem,
                              parsed   = Parsed,
                              runtime  = Time * 1000,
                              port     = Port,
                              received = 0,
                              performers = ordsets:new()}}.

-spec handle_info({port(), any()}, state_name(), state_data())
                 -> {next_state | stop, state_name(), state_data()}.
handle_info({Port, {data, Line}}, running, SD = #state_data{port = Port}) ->
    Current = SD#state_data.received,
    io:format("Erhalte Loesungsvorschlag #~b: ~s", [Current,
                                                      color:green(Line)]),
    NewPerformers = ordsets:add_element(
                      spawn(?MODULE, perform_solution,
                            [Line, SD#state_data.parsed, Current]),
                      SD#state_data.performers),
    {next_state, running, SD#state_data{received = Current + 1,
                                        performers = NewPerformers}};
handle_info({Port, {data, Line}}, warm_up, SD = #state_data{port = Port}) ->
    io:format("~s~n", [color:yellow(Line)]),
    {next_state, warm_up, SD};
handle_info({Port, {exit_status, 0}}, warm_up, SD = #state_data{port = Port}) ->
    io:format("~s~n", [color:red("Program exited unexpectedly but without" ++
                                     " any errors.")]),
    {stop, {exit_early, 0}, SD};
handle_info({Port, {exit_status, ES}}, warm_up,
            SD = #state_data{port = Port}) ->
    io:format("~s~n",
              [color:red("Program exited unexpected and with an error!")]),
    io:format("~s: ~s~n~n", [color:red("Exit code"),
                             color:redb(io_lib:format("~B", [ES]))]),
    {stop, {exit_early, ES}, SD};
handle_info({Port, {exit_status, ES}}, running,
           SD = #state_data{port = Port}) ->
    io:format("Programmende mit ExitCode ~b~n", [ES]),
    {stop, ok, SD};
handle_info(Info, StateName, StateData) ->
    io:format("UNKNOWN: ~p ~n   ~p~n   ~p", [Info, StateName, StateData]),
    {stop, {unknown_event, Info, StateName}, StateData}.

-spec handle_event(any(), state_name(), state_data())
                  -> {stop, tuple(), state_data()}.
handle_event({error, ID, PerfID}, StateName, SD) when
      (StateName == running) or (StateName == collecting) ->
    io:format("--> Nicht parsebare Lösung #~b~n", [ID]),
    proceed_or_stop(StateName, SD, PerfID);
handle_event({invalid, ID, PerfID}, StateName, SD) when
      (StateName == running) or (StateName == collecting) ->
    io:format("--> Ungueltiger Loesungsvorschlag #~b <--~n", [ID]),
    proceed_or_stop(StateName, SD, PerfID);
handle_event({valid, ID, Score, PerfID}, StateName, SD) when
      (StateName == running) or (StateName == collecting) ->
    NewMax = get_max(Score, SD#state_data.max),
    NewMaxID = case get_max(ID, SD#state_data.max_id) of
                   NID when (NID == ID) and (NewMax == Score) ->
                       io:format("--> Loesungsvorschlag #~b wurde mit ~b"
                                 " Punkten bewertet (bisherige Bestleistung ~s)"
                                 "~n", [ID, Score, SD#state_data.max]),
                       ID;
                   NID ->
                       io:format("--> Loesungsvorschlag #~b wurde mit ~b"
                                 " Punkten bewertet (eine neuere Einsendung war"
                                 " bereits besser und wurde vorher ausgewertet)"
                                 "~n", [ID, Score]),
                       NID
               end,
    proceed_or_stop(StateName, SD#state_data{max = NewMax,
                                             max_id = NewMaxID}, PerfID);
handle_event(Event, StateName, SD) ->
    {stop, {unknown_event, Event, StateName}, SD}.

-spec handle_sync_event(any(), {pid(), any()}, state_name(), state_data())
                       -> {stop, tuple(), state_data()}.
handle_sync_event(Event, From, StateName, SD) ->
    {stop, {unknown_event, Event, From, StateName}, SD}.

-spec code_change(any(), state_name(), state_data(), any())
                 -> {ok, state_name(), state_data()}.
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

-spec terminate(any(), any(), any()) -> ok.
terminate(_Reason, _StateName, _StateData) ->
    ok.

%% state handlers

-spec warm_up({countdown, 0}, state_data())
             -> {next_state, running, state_data()};
             ({countdown, pos_integer()}, state_data())
             -> {next_state, warm_up, state_data()}.
warm_up({countdown, 0}, SD) ->
    gen_fsm:send_event_after(SD#state_data.runtime + 100, time_is_over),
    gen_fsm:send_event_after(100, start),
    {next_state, running, SD};
warm_up({countdown, N}, SD) ->
    io:format("~B~n", [N]),
    gen_fsm:send_event_after(1000, {countdown, N - 1}),
    {next_state, warm_up, SD}.

-spec running(start, state_data()) -> {next_state, running, state_data()};
             (time_is_over, state_data()) -> {next_state,
                                              collecting,
                                              state_data()}.
running(start, SD) ->
    port_command(SD#state_data.port, SD#state_data.problem),
    {next_state, running, SD};
running(time_is_over, SD) ->
    port_close(SD#state_data.port),
    case SD#state_data.performers of
        [] -> {stop, ok, SD};
        _  -> {next_state, collecting, SD}
    end.

get_max(Score, undefined) -> Score;
get_max(Score, OldMax) when Score > OldMax -> Score;
get_max(_, OldMax) -> OldMax.

-spec perform_solution(string() | binary(),
                       problem:problem(),
                       non_neg_integer())
                      -> no_return().
perform_solution(InputLine, Problem, ID) ->
    case solution:check(InputLine) of
        {ok, SolutionParsed} ->
            case solution:evaluate(SolutionParsed, Problem) of
                Score when is_integer(Score) ->
                    gen_fsm:send_all_state_event(?MODULE,
                                                 {valid, ID, Score, self()});
                error ->
                    gen_fsm:send_all_state_event(?MODULE, {invalid, ID, self()})
            end;
        _ ->
            gen_fsm:send_all_state_event(?MODULE, {error, ID, self()})
    end.

proceed_or_stop(collecting, SD = #state_data{performers = [PerfID]}, PerfID) ->
    {stop, ok, SD};
proceed_or_stop(StateName, SD, PerfID) ->
    NewPerformers = ordsets:del_element(PerfID, SD#state_data.performers),
    {next_state, StateName, SD#state_data{performers = NewPerformers}}.


%% Local Variables:
%% flycheck-erlang-include-path: ("../include")
%% End:
