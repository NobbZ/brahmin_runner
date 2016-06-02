-module(problem_test).

-include_lib("eunit/include/eunit.hrl").

no_whitespace_test() ->
    ?assertMatch(error, problem:check("[(3,3)]\n[( 3,3,1)]\n1\n")).

perfect_answer_test() ->
    Problem = "[(3,3)]\n[(3,3,10)]\n1\n",
    Solution = "[[(0,0,1)]]\n",
    {ok, ProblemParsed} = problem:check(Problem),
    {ok, SolutionParsed} = solution:check(Solution),
    ?assertMatch(1, solution:evaluate(SolutionParsed, ProblemParsed)).
