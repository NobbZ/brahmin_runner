-module(problem).

-export([check/1]).

-export_type([problem/0]).

-type problem() :: br_exercise:exercise().

-spec check(string() | binary()) -> {ok, br_exercise:exercise()} | error.
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
