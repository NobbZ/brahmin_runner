-module(problem_test).

-include_lib("eunit/include/eunit.hrl").

no_whitespace_test() ->
    ?assertMatch(error, problem:check("[(3,3)]\n[( 3,3,1)]\n1\n")).
