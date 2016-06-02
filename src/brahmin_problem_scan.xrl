Definitions.

DIGIT = [0-9]
WS    = [\s\t]

Rules.

\[ : {token, {'[', TokenLine}}.
\] : {token, {']', TokenLine}}.
\( : {token, {'(', TokenLine}}.
\) : {token, {')', TokenLine}}.
,  : {token, {',', TokenLine}}.
\n : {token, {nl,  TokenLine}}.

-?{DIGIT}+ : {token, {int, TokenLine, list_to_integer(TokenChars)}}.

% {WS}+ : skip_token.

Erlang code.

-dialyzer({nowarn_function, yyrev/2}).

-ignore_xref([format_error/1, string/2, token/2, token/3, tokens/2, tokens/3]).
