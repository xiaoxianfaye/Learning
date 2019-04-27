-module(monitors).
-compile(export_all).

-define(TIMEOUT, 3000).

worker() ->
    receive
        do_work ->
            io:format("I (worker ~p) will work now~n", [self()]),
            worker()
        after ?TIMEOUT ->
            io:format("I (worker ~p) have no work to do~n", [self()]),
            io:format("I (worker ~p) will die now ...~n", [self()]),
            exit(no_activity)
    end.

parent() ->
    Pid = spawn(monitors, worker, []),
    register(worker, Pid),
    Reference = erlang:monitor(process, Pid),

    io:format("I (parent) have a new worker~p~n", [Pid]),
    ?MODULE ! {new_worker, Pid},
    receive
        {'DOWN', Reference, process, Pid, Reason} ->
            io:format("I (parent) My worker ~p died (~p)~n", [Pid, Reason]),
            parent()
    end.

loop() ->
    receive
        {new_worker, WorkerPid} ->
            timer:sleep(?TIMEOUT - 2000),
            WorkerPid ! do_work,
            loop()
    end.

start() ->
    Pid = spawn(monitors, loop, []),
    register(?MODULE, Pid),

    ParentPid = spawn(monitors, parent, []),
    register(parent, ParentPid),

    Ref = erlang:monitor(process, Pid),
    erlang:demonitor(Ref),

    timer:sleep(round(?TIMEOUT * 1.5)),
    exit(whereis(worker), fininshed),
    exit(whereis(parent), fininshed),
    exit(whereis(?MODULE), fininshed),
    ok.