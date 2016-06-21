-module(br_exercise).

-export([new/3, get_bags/1, get_rectangles/1, get_fill_cost/1]).

-export_type([exercise/0]).

-record(exercise, {bags  :: list(br_bag:bag()),
                   rects :: list(br_rectangle:rectangle()),
                   fill  :: non_neg_integer()}).

-type exercise() :: #exercise{}.

-spec new(list(br_bag:bag()), list(br_rectangle:rectangle()), non_neg_integer())
         -> exercise().
new(Bags, Rects, Fill) ->
    #exercise{bags  = Bags,
              rects = Rects,
              fill  = Fill}.

-spec get_bags(exercise()) -> list(br_bag:bag()).
get_bags(#exercise{bags = B}) -> B.

-spec get_rectangles(exercise()) -> list(br_rectangle:rectangle()).
get_rectangles(#exercise{rects = R}) -> R.

-spec get_fill_cost(exercise()) -> non_neg_integer().
get_fill_cost(#exercise{fill = F}) -> F.

%% Local Variables:
%% flycheck-erlang-include-path: ("../include")
%% End:
