-module(brahmin_runner).

%% API exports
-export([main/1, foo/1]).

%% Functions exposed for beeing used from outside of the OTP-app
-ignore_xref([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec(main(list()) -> no_return()).
main([]) ->
    do_error(args_missing);
main(["validate", ProblemName]) ->
    Problem = case file:read_file("problems/" ++ ProblemName ++ ".prb") of
                  {ok, P} -> binary_to_list(P);
                  {error, _} -> do_error(file_error)
              end,
    case problem:check(Problem) of
        {ok, _} -> io:format("No errors in problemdescription!~n"),
                   erlang:halt(0);
        error -> {T, S} = get_error_info(invalid_problem),
                 io:format("~s~n", [T]),
                 erlang:halt(S)
    end;
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
    ParsedProblem = case problem:check(Problem) of
                        {ok, Value} -> Value;
                        error -> do_error(invalid_problem)
                    end,
    %Runner = spawn(runner, run, [Problem, ParsedProblem, Time]),
    {ok, Runner} = runner:run(Problem, ParsedProblem, Time),
    io:format("~p~n", [Runner]),
    erlang:monitor(process, Runner),
    io:format("~p~n", [Runner]),
                                                %timer:exit_after(Time * 1000, Runner, "Timeout!"),
    observer:start(),
    %timer:sleep(5000),
    receive
        {'DOWN', _, process, Runner, Reason} ->
            ok %io:format("~p~n", [Reason])
    end,
    erlang:halt(0);
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    erlang:halt(0).

foo(N) ->
    io:format("~p: ~p~n", [self(), N]),
    timer:sleep(1000),
    foo(N + 1).

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
get_error_info(file_error)   -> {"File error", 3};
get_error_info(invalid_problem) -> {"There are errors in the problem description", 4}.

print_help() ->
    io:format("~s: run a problem~n", [color:yellowb("OPTION A")]),
    io:format("~s~n", [color:yellow("========")]),
    io:format("./~s <~s> <~s>~n~n", [color:cyanb("brahmin_runner"),
                                     color:blue("time"),
                                     color:blue("problem")]),
    io:format("~s~n~n", [color:blueb("Arguments:")]),
    io:format("  <~s>   : Time in the seconds the program is allowed to run~n", [color:blue("time")]),
    io:format("  <~s>: The name of the problem to run~n~n", [color:blue("problem")]),
    io:format("The <~s> is loaded from subfolder \"problems\", extension has to be \".prb\".~n", [color:blue("problem")]),

    io:format("~n~n~s: validate a problem~n", [color:yellowb("OPTION B")]),
    io:format("~s~n", [color:yellow("========")]),
    io:format("./~s ~s <~s>~n~n", [color:cyanb("brahmin_runner"),
                                   color:cyan("validate"),
                                   color:blue("problem")]),
    io:format("~s~n~n", [color:blueb("Argument:")]),
    io:format("  <~s>: The name of the problem to validate~n~n", [color:blue("problem")]),
    io:format("The <~s> is loaded from subfolder \"problems\", extension has to be \".prb\".~n", [color:blue("problem")]).

%% Local Variables:
%% flycheck-erlang-include-path: ("../include")
%% End:
