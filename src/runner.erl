-module(runner).

-export([run/3]).

-behaviour(gen_fsm).
-export([init/1]).

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
    gen_fsm:send_event_after(1000, {countdown, 5}),
    {ok, warm_up, #state_data{problem = Problem,
                              parsed  = Parsed,
                              runtime = Time * 1000}}.

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
