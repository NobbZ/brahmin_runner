Nonterminals solution bags bag rectangles rectangle x y id.

Terminals '[' ']' '(' ')' ',' nl int.

Rootsymbol solution.

solution -> '[' ']' nl      : solution:new([]).
solution -> '[' bags ']' nl : solution:new('$2').

bags -> bag          : ['$1'].
bags -> bag ',' bags : ['$1'|'$3'].

bag -> '[' ']'            : [].
bag -> '[' rectangles ']' : '$2'.

rectangles -> rectangle                : ['$1'].
rectangles -> rectangle ',' rectangles : ['$1'|'$3'].

rectangle -> '(' x ',' y ',' id ')' : #rec_ref{x = '$2', y = '$4', id = '$6'}.

x  -> int : extract('$1').
y  -> int : extract('$1').
id -> int : extract('$1').

Erlang code.

-include("rec_ref.hrl").

-export([parse_string/1]).

-ignore_xref([format_error/1, parse_and_scan/1, parse/1]).

extract({_Token, _Pos, Value}) -> Value.

parse_string(String) when is_binary(String) ->
    parse_string(binary_to_list(String));
parse_string(String) when is_list(String) ->
    case brahmin_solution_scan:string(String) of
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
