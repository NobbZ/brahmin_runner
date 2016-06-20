-module(br_exercise).

-export([new/3, get_bags/1]).

-export_type([exercise/0]).

-record(exercise, {bags  :: list(br_bag:bag()),
                   rects :: list(br_rect:rect()),
                   fill  :: non_neg_integer()}).

-type exercise() :: #exercise{}.

-spec new(list(br_bag:bag()), list(br_rect:rect()), non_neg_integer())
         -> exercise().
new(Bags, Rects, Fill) ->
    #exercise{bags  = Bags,
              rects = Rects,
              fill  = Fill}.

-spec get_bags(exercise()) -> list(br_bag:bag()).
get_bags(#exercise{bags = B}) -> B.

%% Local Variables:
%% flycheck-erlang-include-path: ("../include")
%% End:
