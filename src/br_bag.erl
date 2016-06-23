-module(br_bag).

-export([new/2, get_area/1, can_sonsume/2]).

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

-spec can_sonsume(bag(), {X :: non_neg_integer(),
                          Y :: non_neg_integer(),
                          Width :: non_neg_integer(),
                          Height :: non_neg_integer()})
                 -> boolean().
can_sonsume(#bag{width = BW, height = BH}, {X, Y, W, H}) ->
    (X + W =< BW) and (Y + H =< BH).
