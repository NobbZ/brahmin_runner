-module(br_bag).

-export([new/2, get_area/1]).

-export_type([bag/0]).

-record(bag, {width  :: non_neg_integer(),
              height :: non_neg_integer()}).

-type bag() :: #bag{}.

-spec new(non_neg_integer(), non_neg_integer()) -> bag().
new(Width, Height) ->
    #bag{width  = Width,
         height = Height}.

-spec get_area(bag()) -> non_neg_integer().
get_area(#bag{width = W, height = H}) -> W * H.
