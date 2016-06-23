-module(br_rectangle).

-export([new/3, get_x/1, get_y/1, get_width/1, get_height/1, get_value/1,
         get_size/1, new_ref/3, get_id/1, fill_context/2, overlap/2]).

-export_type([rectangle/0, ref/0]).

-record(rectangle, {width  :: non_neg_integer(),
                    height :: non_neg_integer(),
                    value  :: integer()}).

-type rectangle() :: #rectangle{}.

-record(rec_ref, {x  ::  non_neg_integer(),
                  y  ::  non_neg_integer(),
                  id :: non_neg_integer()}).
-type ref() :: #rec_ref{}.

-spec new(non_neg_integer(), non_neg_integer(), integer()) -> rectangle().
new(Width, Height, Value) ->
    #rectangle{width  = Width,
               height = Height,
               value  = Value}.

-spec new_ref(non_neg_integer(), non_neg_integer(), non_neg_integer()) -> ref().
new_ref(ID, X, Y) ->
    #rec_ref{id = ID, x = X, y = Y}.

-spec get_id(ref()) -> non_neg_integer().
get_id(#rec_ref{id = ID}) -> ID.

-spec get_x(ref()) -> non_neg_integer().
get_x(#rec_ref{x = X}) -> X.

-spec get_y(ref()) -> non_neg_integer().
get_y(#rec_ref{y = Y}) -> Y.

-spec get_width(rectangle()) -> non_neg_integer().
get_width(#rectangle{width = W}) -> W.

-spec get_height(rectangle()) -> non_neg_integer().
get_height(#rectangle{height = H}) -> H.

-spec get_value(rectangle()) -> non_neg_integer().
get_value(#rectangle{value = V}) -> V.

-spec get_size(rectangle()) -> non_neg_integer().
get_size(#rectangle{width = W, height = H}) -> W * H.

-spec fill_context(list(ref()), list(rectangle()))
                  -> list({ref(), rectangle()}).
fill_context([], _) -> [];
fill_context([Ref = #rec_ref{}|T], Rects) when is_list(Rects) ->
    Rec = lists:nth(
            Ref#rec_ref.id + 1, % Erlang lists are indexed by 1…
            Rects),
    [{Ref, Rec}|fill_context(T, Rects)].

-spec overlap({ref(), rectangle()}, {ref(), rectangle()}) -> boolean().
overlap({#rec_ref{x = X1, y = Y1}, #rectangle{width = W1, height = H1}},
        {#rec_ref{x = X2, y = Y2}, #rectangle{width = W2, height = H2}}) ->
    not ((X1 + W1 =< X2) or (X1 >= X2 + W2)
         or (Y1 + H1 =< Y2) or (Y1 >= Y2 + H2)).
