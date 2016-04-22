-module(brahmin_runner).

%% API exports
-export([main/1]).

%% Functions exposed for beeing used from outside of the OTP-app
-ignore_xref([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([]) ->
    print_help();
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

print_help() ->
    io:format("./~s <~s> <~s>~n~n", [color:cyanb("brahmin_runner"),
                                     color:blue("time"),
                                     color:blue("problem")]),
    io:format("~s~n~n", [color:blueb("Arguments:")]),
    io:format("  <~s>   : Time in the seconds the program is allowed to run~n", [color:blue("time")]),
    io:format("  <~s>: The name of the problem to run~n~n", [color:blue("problem")]),
    io:format("The <~s> is loaded from subfolder \"problems\", extension has to be \".prb\".~n", [color:blue("problem")]).
