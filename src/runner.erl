-module(runner).

-export([run/3]).

-behaviour(gen_fsm).
-export([init/1, handle_info/3, handle_event/3, handle_sync_event/4,
         code_change/4, terminate/3]).

-ignore_xref([warm_up/2, running/2]).
-export([warm_up/2, running/2]).

-record(state_data, {
          problem,
          parsed,
          runtime,
          port
         }).

%% public API
run(Problem, Parsed, Time) ->
    gen_fsm:start({local, ?MODULE}, ?MODULE, {Problem, Parsed, Time}, []).

%% gen_fsm API

init({Problem, Parsed, Time}) ->
    gen_fsm:send_event_after(1000, {countdown, 5}),
    Make = os:find_executable("make"),
    io:format("~p~n", [Make]),
    Port = open_port({spawn_executable, Make}, [{args, [<<"run">>]},
                                                exit_status]),
    {ok, warm_up, #state_data{problem = Problem,
                              parsed  = Parsed,
                              runtime = Time * 1000,
                              port    = Port}}.

handle_info({Port, {data, Line}}, running, SD) ->
    io:format("~s~n", [color:green(Line)]),
    {next_state, running, SD};
handle_info({Port, {data, Line}}, warm_up, SD) ->
    io:format("~s~n", [color:yellow(Line)]),
    {next_state, warm_up, SD};
handle_info({Port, {exit_status, 0}}, warm_up, SD) ->
    io:format("~s~n", [color:red("Program exited unexpectedly but without" ++
                                     " any errors.")]),
    {stop, {exit_early, 0}, SD};
handle_info({Port, {exit_status, ES}}, warm_up, SD) ->
    io:format("~s~n",
              [color:red("Program exited unexpected and with an error!")]),
    io:format("~s: ~s~n~n", [color:red("Exit code"),
                             color:redb(io_lib:format("~B", [ES]))]),
    {stop, {exit_early, ES}, SD}.

handle_event(Event, StateName, SD) ->
    {stop, {unknown_event, Event, StateName}, SD}.

handle_sync_event(Event, From, StateName, SD) ->
    {stop, {unknown_event, Event, From, StateName}, SD}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

%% state handlers

warm_up({countdown, 0}, SD) ->
    gen_fsm:send_event_after(SD#state_data.runtime * 1000 + 100, time_is_over),
    gen_fsm:send_event_after(100, start),
    {next_state, running, SD};
warm_up({countdown, N}, SD) ->
    io:format("~B~n", [N]),
    gen_fsm:send_event_after(1000, {countdown, N - 1}),
    {next_state, warm_up, SD}.

running(start, SD) ->
    port_command(SD#state_data.port, SD#state_data.problem),
    {next_state, running, SD}.

%% Local Variables:
%% flycheck-erlang-include-path: ("../include")
%% End:
