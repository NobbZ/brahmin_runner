-module(brahmin_runner).

%% API exports
-export([main/1]).

%% Functions exposed for beeing used from outside of the OTP-app
-ignore_xref([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec(main(list()) -> no_return()).
main([]) ->
    do_error(args_missing);
main(["help"]) ->
    print_help(),
    erlang:halt(0);
main([_]) ->
    do_error(args_missing);
main([TimeStr, ProblemName]) ->
    Time = case string:to_integer(TimeStr) of
               {N, []} -> N;
               {_, _} -> do_error(invalid_time)
           end,
    Problem = case file:read_file("problems/" ++ ProblemName ++ ".prb") of
                  {ok, P} -> binary_to_list(P);
                  {error, _} -> do_error(file_error)
              end,
    case problem:check(Problem) of
        ok -> ok;
        error -> do_error(invalid_problem)
    end;
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

do_error(ErrorType) ->
    {Message, Status} = get_error_info(ErrorType),
    io:format("~s~n~n", [color:redb(Message)]),
    print_help(),
    erlang:halt(Status).

get_error_info(args_missing) -> {"Missing arguments", 1};
get_error_info(invalid_time) -> {"Time is not an integer", 2};
get_error_info(file_error)   -> {"File error", 3}.

print_help() ->
    io:format("./~s <~s> <~s>~n~n", [color:cyanb("brahmin_runner"),
                                     color:blue("time"),
                                     color:blue("problem")]),
    io:format("~s~n~n", [color:blueb("Arguments:")]),
    io:format("  <~s>   : Time in the seconds the program is allowed to run~n", [color:blue("time")]),
    io:format("  <~s>: The name of the problem to run~n~n", [color:blue("problem")]),
    io:format("The <~s> is loaded from subfolder \"problems\", extension has to be \".prb\".~n", [color:blue("problem")]).

%% Local Variables:
%% flycheck-erlang-include-path: ("../include")
%% End:
