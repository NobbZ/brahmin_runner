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

{WS}+ : skip_token.

Erlang code.
