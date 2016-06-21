Nonterminals exercise bags rectangles fill_price bag rectangle width height value.

Terminals '[' ']' '(' ')' ',' nl int.

Rootsymbol exercise.

exercise ->
    '[' bags ']' nl '[' rectangles ']' nl fill_price nl : br_exercise:new('$2', '$6', '$9').

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
    '(' width ',' height ')' : br_bag:new('$2', '$4').

rectangle ->
    '(' width ',' height ',' value ')' : br_rectangle:new('$2', '$4', '$6').

width  -> int : extract('$1').
height -> int : extract('$1').
value  -> int : extract('$1').

Erlang code.

-export([parse_string/1]).

-ignore_xref([format_error/1, parse_and_scan/1, parse/1, return_error/2]).

-dialyzer([{nowarn_function,
            [parse/1, parse_and_scan/1, format_error/1]}]).

extract({_Token, _Pos, Value}) -> Value.

parse_string(String) when is_binary(String) ->
    parse_string(binary_to_list(String));
parse_string(String) when is_list(String) ->
    FirstLines = get_first_lines(String, 3),
    case brahmin_problem_scan:string(FirstLines) of
        {ok, Tokens, _} ->
            case parse(Tokens) of
                {ok, Parsed} ->
                    {ok, Parsed};
                {error, {Line, _, Message}} ->
                    {error, Line, Message}
            end;
        {error, {Line, _, {illegal, S}}, _} ->
            {error, Line, "Illegal token: " ++ S}
    end.

get_first_lines(Input, Count) ->
    get_first_lines(Input, Count, []).

get_first_lines([], _, Acc) ->
    lists:reverse(Acc);
get_first_lines(_, 0, Acc) ->
    lists:reverse(Acc);
get_first_lines([$\n|T], Count, Acc) ->
    get_first_lines(T, Count - 1, [$\n|Acc]);
get_first_lines([H|T], Count, Acc) ->
    get_first_lines(T, Count, [H|Acc]).
