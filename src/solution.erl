-module(solution).

-export([new/1, check/1, evaluate/2]).

-export_type([solution/0]).

-record(solution, {bags :: list(cr_bag:bag())}).
-type solution() :: #solution{}.

-spec new(list(cr_bag:bag())) -> solution().
new(Bags) ->
    #solution{bags = Bags}.

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
