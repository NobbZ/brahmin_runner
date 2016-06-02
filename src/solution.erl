-module(solution).

-export([check/1]).

check(Solution) when is_list(Solution); is_binary(Solution) ->
    case brahmin_solution_parse:parse_string(Solution) of
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
