-module(br_rectangle).

-export([new/3]).

-export_type([rectangle/0]).

-record(rectangle, {width  :: non_neg_integer(),
                    height :: non_neg_integer(),
                    value  :: integer()}).

-type rectangle() :: #rectangle{}.

-spec new(non_neg_integer(), non_neg_integer(), integer()) -> rectangle().
new(Width, Height, Value) ->
    #rectangle{width  = Width,
               height = Height,
               value  = Value}.
