-module(runner).

-export([run/3]).

-behaviour(gen_fsm).
-export([init/1, handle_info/3, terminate/3]).

-ignore_xref([warm_up/2]).
-export([warm_up/2]).

-record(state_data, {
          problem,
          parsed,
          runtime
         }).

%% public API
run(Problem, Parsed, Time) ->
    gen_fsm:start({local, ?MODULE}, ?MODULE, {Problem, Parsed, Time}, []).

%% gen_fsm API

init({Problem, Parsed, Time}) ->
    gen_fsm:send_event_after(1000, {countdown, 10}),
    Make = os:find_executable("make"),
    io:format("~p~n", [Make]),
    open_port({spawn_executable, Make}, [{args, [<<"run">>]},
                                         exit_status]),
    {ok, warm_up, #state_data{problem = Problem,
                              parsed  = Parsed,
                              runtime = Time * 1000}}.

handle_info({Port, {data, Line}}, warm_up, SD) ->
    io:format("~s~n", [color:yellow(Line)]),
    {next_state, warm_up, SD};
handle_info({Port, {exit_status, 0}}, warm_up, SD) ->
    io:format("~s~n", [color:red("Program exited unexpectedly but without any errors.")]),
    {stop, {exit_early, 0}, SD};
handle_info({Port, {exit_status, ES}}, warm_up, SD) ->
    io:format("~s~n", [color:red("Program exited unexpected and with an error!")]),
    io:format("~s: ~s~n~n", [color:red("Exit code"),
                             color:redb(io_lib:format("~B", [ES]))]),
    {stop, {exit_early, ES}, SD}.

terminate(Reason, StateName, StateData) ->
    ok.


%% state handlers

warm_up({countdown, 0}, SD) ->
    {stop, 'ended', SD};
warm_up({countdown, N}, SD) ->
    io:format("~B~n", [N]),
    gen_fsm:send_event_after(1000, {countdown, N - 1}),
    {next_state, warm_up, SD}.

%% Local Variables:
%% flycheck-erlang-include-path: ("../include")
%% End:
