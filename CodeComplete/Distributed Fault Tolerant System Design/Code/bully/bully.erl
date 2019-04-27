-module(bully).
-compile(export_all).

start(I, N) ->
    Pid = spawn(fun() -> init(I, N) end),
    Pid ! start.

stop(I) ->
    send_to([I], stop).

send_to(ProcNos, Msg) ->
    [case whereis(proc_name(I)) of
        undefined ->
            pass;
        Pid ->
            Pid ! Msg
     end || I <- ProcNos].

proc_name(I) ->
    list_to_atom("p_" ++ integer_to_list(I)).

init(I, N) ->
    io:format("process ~p started~n", [I]),
    register(proc_name(I), self()),
    idle(I, N).

idle(I, N) ->
    receive
        start when I =:= N ->
            io:format("process ~p is leader~n", [I]),
            send_to(lists:seq(0, N - 1), {victory, self()}),
            working(I, N, self());
        start when I < N ->
            send_to(lists:seq(I + 1, N), {election, I, self()}),
            erlang:send_after(2000, self(), waiting_answers_timeout),
            election(I, N, 0)
    end.

election(I, N, Answers) ->
    receive
        waiting_answers_timeout when Answers =:= 0 ->
            io:format("process ~p is leader~n", [I]),
            send_to(lists:seq(0, N) -- [I], {victory, self()}),
            working(I, N, self());
        {answer, ProcID} when ProcID > I ->
            erlang:send_after(5000, self(), waiting_victory_timeout),
            waiting(I, N);
        {victory, NewLeader} ->
            erlang:monitor(process, NewLeader),
            working(I, N, NewLeader);
        {election, ProcNo, Pid} when ProcNo < I ->
            Pid ! {answer, I},
            election(I, N, Answers)
    end.

waiting(I, N) ->
    receive
        {victory, NewLeader} ->
            erlang:monitor(process, NewLeader),
            working(I, N, NewLeader);
        waiting_victory_timeout ->
            self() ! start,
            idle(I, N);
        {election, ProcNo, Pid} when ProcNo < I ->
            Pid ! {answer, I},
            waiting(I, N);
        {answer, ProcID} when ProcID > I ->
            waiting(I, N)
    end.

working(I, N, Leader) ->
    receive
        {election, _ProcNo, Pid} when I =:= N ->
            Pid ! {answer, I},
            working(I, N, Leader);
        {election, ProcNo, Pid} when ProcNo < I ->
            Pid ! {answer, I},
            self() ! start,
            idle(I, N);
        {victory, NewLeader} when NewLeader =/= Leader ->
            erlang:monitor(process, NewLeader),
            working(I, N, NewLeader);
        {'DOWN', _, process, Leader, _} ->
            io:format("process ~p found leader down~n", [I]),
            self() ! start,
            idle(I, N);
        stop ->
            io:format("process ~p stopped~n", [I]),
            stop;
        _ ->
            working(I, N, Leader)
    end.

test(N) ->
    [start(I, N) || I <- lists:seq(0, N)],
    ok.