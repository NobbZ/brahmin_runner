-module(problem).

-export([check/1]).

check(Problem) when is_list(Problem) ->
    ok;
check(_) ->
    error.
