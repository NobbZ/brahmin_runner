Nonterminals exercise bags rectangles fill_price bag rectangle width height value.

Terminals '[' ']' '(' ')' ',' nl int.

Rootsymbol exercise.

exercise ->
    '[' bags ']' nl '[' rectangles ']' nl fill_price nl : #exercise{bags = '$2',
                                                                    rects = '$6',
                                                                    fill = '$9'}.

bags ->
    bag : ['$1'].
bags ->
    bag ',' bags : ['$1'|'$3'].

rectangles ->
    rectangle : ['$1'].
rectangles ->
    rectangle ',' rectangles : ['$1'|'$3'].

fill_price ->
    int : extract('$1').

bag ->
    '(' width ',' height ')' : #bag{width = '$2', height = '$4'}.

rectangle ->
    '(' width ',' height ',' value ')' : #rectangle{width = '$2', height = '$4', value = '$6'}.

width  -> int : extract('$1').
height -> int : extract('$1').
value  -> int : extract('$1').

Erlang code.

-include("bag.hrl").
-include("rectangle.hrl").
-include("exercise.hrl").

-export([parse_string/1]).

extract({_Token, _Pos, Value}) -> Value.

parse_string(String) when is_binary(String) ->
    parse_string(binary_to_list(String));
parse_string(String) when is_list(String) ->
    FirstLines = get_first_lines(String, 3),
    {ok, Tokens, _} = brahmin_problem_scan:string(FirstLines),
    case parse(Tokens) of
        {ok, Parsed} ->
            {ok, Parsed};
        {error, {Line, _, Message}}Other ->
            {error, Line, Message}
    end.

get_first_lines(Input, Count) ->
    get_first_lines(Input, Count, []).

get_first_lines([], _, Acc) ->
    lists:reverse(acc);
get_first_lines(_, 0, Acc) ->
    lists:reverse(Acc);
get_first_lines([$\n|T], Count, Acc) ->
    get_first_lines(T, Count - 1, [$\n|Acc]);
get_first_lines([H|T], Count, Acc) ->
    get_first_lines(T, Count, [H|Acc]).
