-module(if_else).
-compile(export_all).

compare(X, Y) ->
    Result = if
        X > Y -> greater;
        X == Y -> equal;
        X < Y -> less
    end,
    io:format("~p is ~p than ~p ~n", [X, Result, Y]).

ascii(Letter) ->
    Code = if
        Letter =:= "A" -> 101;
        Letter =:= "B" -> 102;
        true -> unknown
    end,
    io:format("~p = ~p~n", [Letter, Code]).

run() ->
    compare(5, 1),
    ascii("A").