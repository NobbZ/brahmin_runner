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

-compile(no_warn_missing_spec).

-dialyzer({nowarn_function, yyrev/2}).

-ignore_xref([format_error/1, string/2, token/2, token/3, tokens/2, tokens/3]).

% There are no specs defined for the following types, so compiling does emit a
% warning, despite of the flag provided above.
-spec format_error({illegal, string()}) -> _;
                  ({user,    _})        -> _.
-spec string(_) -> {error, {_, brahmin_solution_scan, {_, _}}, _} |
                   {ok, [any()], _}.
-spec string(_,_) -> {error, {_, brahmin_solution_scan, {_, _}}, _} |
                     {ok, [any()], _}.
-spec token([] | {token, _, _, [any()], _, _, _, _}, _)
           -> {more, {token, _, _, _, _, _, _, _}} |
              {done, {eof, _} |
                     {error, {_, _, _}, _} |
                     {ok, {_, _} | {_, _, _}, _}, _}.
-spec token([] | {token, _, _, [any()], _, _, _, _}, _, _)
           -> {more, {token, _, _, _, _, _, _, _}} |
              {done, {eof, _} |
                     {error, {_, _, _}, _} |
                     {ok, {_, _} | {_, _, _}, _}, _}.
-spec tokens([] |
             {skip_tokens, _, _, [any()], _, _, _, _, _} |
             {tokens, _, _, [any()], _, _, _, _, _}, _)
            -> {more, {skip_tokens, _, _, _, _, _, _, _, _} |
                      {tokens, _, _, _, _, _, _, _, _}} |
               {done, {eof, _} |
                      {error, _, _} |
                      {ok, [any()], _}, eof}.
-spec tokens([] |
             {skip_tokens, _, _, [any()], _, _, _, _, _} |
             {tokens, _, _, [any()], _, _, _, _, _}, _, _)
            -> {more, {skip_tokens, _, _, _, _, _, _, _, _} |
                      {tokens, _, _, _, _, _, _, _, _}} |
               {done, {eof, _} |
                      {error, _, _} |
                      {ok, [any()], _}, eof}.
