-module(problem).

-export([check/1]).

check(Problem) when is_list(Problem); is_binary(Problem) ->
    case brahmin_problem_parse:parse_string(Problem) of
        {ok, Parsed} ->
            {ok, Parsed};
        {error, Line, Message} ->
            io:format("Error on line ~s: ~s~n~n",
                      [color:blueb(io_lib:format("~B", [Line])),
                       color:yellow(Message)]),
            error
    end;
check(_) ->
    error.
