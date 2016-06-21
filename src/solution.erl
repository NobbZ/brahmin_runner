-module(solution).

-export([new/1, check/1, evaluate/2]).

-export_type([solution/0, reflist/0]).

-type reflist() :: list(list(br_rectangle:ref())).

-record(solution, {refs :: reflist()}).
-type solution() :: #solution{}.

-spec new(reflist()) -> solution().
new(Bags) ->
    #solution{refs = Bags}.

-spec check(string() | binary()) -> {ok, solution()} | error.
check(Solution) when is_list(Solution); is_binary(Solution) ->
    case brahmin_solution_parse:parse_string(Solution) of
        {ok, Parsed} ->
            {ok, Parsed};
        {error, Line, Message} ->
            io:format("Error on line ~s: ~s~n~n",
                      [color:blueb(io_lib:format("~B", [Line])),
                       color:yellow(Message)]),
            error
    end;
check(_) ->
    error.

-spec evaluate(solution(), br_exercise:exercise()) -> integer() | error.
evaluate(Solution, Problem) ->
    BagSpaces = bag_spaces(br_exercise:get_bags(Problem)),
    Rectangles = br_exercise:get_rectangles(Problem),
    SortedSolution = lists:map(fun sort_by_ref/1, Solution#solution.refs),
    FillContext = lists:map(fun (Refs) -> br_rectangle:fill_context(Refs, Rectangles) end, SortedSolution),
    ValidatedTimeAndSpace = go_to_tardis(FillContext, br_exercise:get_bags(Problem)),
    if ValidatedTimeAndSpace ->
            Score = sum_up_context(FillContext, 0),
            FillNeeded = fill_up(lists:sum(BagSpaces), FillContext),
            Score + FillNeeded * -br_exercise:get_fill_cost(Problem);
       true ->
            error
    end.
%    BagScores = score_bags(SortedSolution, Problem#exercise.bags, BagSpaces),
    %BagSpaces.

-spec bag_spaces(list(br_bag:bag())) -> [non_neg_integer()].
bag_spaces([]) -> [];
bag_spaces([H|T]) -> [br_bag:get_area(H)|bag_spaces(T)].

%% score_bags([], [], [], Sum, Empty) -> {ok, Sum, Empty};
%% score_bags([RectRef|SolT], [Bag|BagT], [Space|]) ->

fill_up(EmptySpace, []) -> EmptySpace;
fill_up(EmptySpace, [[]|T]) -> fill_up(EmptySpace, T);
fill_up(EmptySpace, [[{_, Rect}|IT]|OT]) ->
    fill_up(EmptySpace - br_rectangle:get_size(Rect), [IT|OT]).

sort_by_ref(List) ->
    lists:sort(
      fun(A, B) ->
              br_rectangle:get_id(A) =< br_rectangle:get_id(B)
      end,
      List).

go_to_tardis([], _) -> true;
go_to_tardis([[]|CT], [_|BT])-> go_to_tardis(CT, BT);
go_to_tardis([[{Ref, Rect}|Others]|CT], [Bag|BT]) ->
    Fits = br_bag:can_sonsume(Bag, {br_rectangle:get_x(Ref),
                                    br_rectangle:get_y(Ref),
                                    br_rectangle:get_width(Rect),
                                    br_rectangle:get_height(Rect)}),
    NoOverlaps = lists:all(fun (R) -> not br_rectangle:overlap({Ref, Rect}, R) end, Others),
    io:format("{~p, ~p} in ~p~n --> Bag ~p~n --> Fits: ~p~n --> BT: ~p~n --> CT: ~p~n --> NoOv: ~p~n", [Ref, Rect, Others, Bag, Fits, BT, CT, NoOverlaps]),
    Fits and NoOverlaps andalso go_to_tardis([Others|CT], [Bag|BT]).

sum_up_context([], Acc) -> Acc;
sum_up_context([[]|T], Acc) -> sum_up_context(T, Acc);
sum_up_context([[{_, Rect}|TI]|TO], Acc) ->
    sum_up_context([TI|TO], Acc + br_rectangle:get_value(Rect)).

%% Local Variables:
%% flycheck-erlang-include-path: ("../include")
%% End:
