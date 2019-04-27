-module(send_recv).
-compile(export_all).

serve() ->
    receive
        Request ->
            io:format("Handling: ~s~n", [Request]),
            serve()
    end.

math() ->
    receive
        {add, X, Y} ->
            io:format("~p + ~p = ~p~n", [X, Y, X + Y]),
            math();
        {sub, X, Y} ->
            io:format("~p - ~p = ~p~n", [X, Y, X - Y]),
            math()
    end.

make_request(ServerId, Msg) ->
    ServerId ! Msg.

run() ->
    Pid = spawn(?MODULE, serve, []),
    make_request(Pid, request1),
    make_request(Pid, request2),

    timer:sleep(10),

    Pid2 = spawn(?MODULE, math, []),
    Pid2 ! {add, 1, 2},
    Pid2 ! {sub, 3, 2},
    ok.