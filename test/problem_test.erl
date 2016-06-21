-module(problem_test).

-include_lib("eunit/include/eunit.hrl").

no_whitespace_test() ->
    ?assertMatch(error, problem:check("[(3,3)]\n[( 3,3,1)]\n1\n")).

perfect_answer_test() ->
    Problem = "[(3,3)]\n[(3,3,10)]\n1\n",
    Solution = "[[(0,0,0)]]\n",
    {ok, ProblemParsed} = problem:check(Problem),
    {ok, SolutionParsed} = solution:check(Solution),
    ?assertMatch(10, solution:evaluate(SolutionParsed, ProblemParsed)).

another_answer_test() ->
    Problem = "[(2,2)]\n[(1,2,1),(1,2,1)]\n0\n",
    Solution = "[[(0,0,1),(1,0,0)]]\n",
    {ok, ProblemParsed} = problem:check(Problem),
    {ok, SolutionParsed} = solution:check(Solution),
    ?assertMatch(2, solution:evaluate(SolutionParsed, ProblemParsed)).

outputformat_first_example_test() ->
    Problem = "[(3,3)]\n[(2,2,1),(2,1,1),(1,2,1)]\n0\n",
    Solution = "[[(0,0,0),(0,2,1),(2,0,2)]]\n",
    {ok, ProblemParsed} = problem:check(Problem),
    {ok, SolutionParsed} = solution:check(Solution),
    ?assertMatch(3, solution:evaluate(SolutionParsed, ProblemParsed)).

outputformat_first_example_second_possibility_test() ->
    Problem = "[(3,3)]\n[(2,2,4),(2,1,2),(1,3,3)]\n0\n",
    Solution = "[[(2,0,2),(0,2,1),(0,0,0)]]\n",
    {ok, ProblemParsed} = problem:check(Problem),
    {ok, SolutionParsed} = solution:check(Solution),
    ?assertMatch(9, solution:evaluate(SolutionParsed, ProblemParsed)).

outputformat_second_example_test() ->
    Problem = "[(1,2),(1,3)]\n[(1,2,1),(1,3,1)]\n0\n",
    Solution = "[[(0,0,0)],[(0,0,1)]]\n",
    {ok, ProblemParsed} = problem:check(Problem),
    {ok, SolutionParsed} = solution:check(Solution),
    ?assertMatch(2, solution:evaluate(SolutionParsed, ProblemParsed)).

five_rects_in_nine_squares_test() ->
    Problem = "[(3,3)]\n[(1,2,5),(1,2,5),(2,1,5),(2,1,5),(1,1,5)]\n5\n",
    Solution = "[[(0,0,0),(2,1,1),(1,0,3),(0,2,2),(1,1,4)]]\n",
    {ok, ProblemParsed} = problem:check(Problem),
    {ok, SolutionParsed} = solution:check(Solution),
    ?assertMatch(25, solution:evaluate(SolutionParsed, ProblemParsed)).

apply_filling_mats_test() ->
    Problem = "[(3,3)]\n[(1,2,5),(1,2,5),(2,1,5),(2,1,5),(1,1,5)]\n2\n",
    Solution = "[[(0,0,0),(2,1,1),(1,0,3)]]\n",
    {ok, ProblemParsed} = problem:check(Problem),
    {ok, SolutionParsed} = solution:check(Solution),
    ?assertMatch(9, solution:evaluate(SolutionParsed, ProblemParsed)).

negative_value_test() ->
    Problem = "[(3,3)]\n[(3,3,-10)]\n2\n",
    Solution = "[[(0,0,0)]]\n",
    {ok, ProblemParsed} = problem:check(Problem),
    {ok, SolutionParsed} = solution:check(Solution),
    ?assertMatch(-10, solution:evaluate(SolutionParsed, ProblemParsed)).
