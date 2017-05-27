-module(stream_p).
-compile(export_all).

-type stream() :: pid() | [].
-spec cons_stream(term(), fun((stream()) -> stream())) -> stream().

cons_stream(H, T) ->
    spawn(fun() -> stream_idle(H, T) end).

head(S) ->
    Ref = make_ref(),
    S ! {ask_head, Ref, self()},
    receive
        {head, Ref, H} ->
            H
    end.

tail(S) ->
    Ref = make_ref(),
    S ! {ask_tail, Ref, self()},
    receive
        {tail, Ref, T} ->
            T
    end.

stream_idle(H, TF) ->
    receive
        {ask_head, Ref, Pid} ->
            Pid ! {head, Ref, H},
            stream_idle(H, TF);
        {ask_tail, Ref, Pid} ->
            S = self(),
            spawn(
                fun() ->
                    TR = TF(S),
                    S ! {tail_ready, TR}
                end),
            stream_tl_waiting(H, [{Pid, Ref}])
    end.

stream_tl_waiting(H, TWaits) ->
    receive
        {ask_head, Ref, Pid} ->
            Pid ! {head, Ref, H},
            stream_tl_waiting(H, TWaits);
        {ask_tail, Ref, Pid} ->
            stream_tl_waiting(H, [{Pid, Ref} | TWaits]);
        {tail_ready, TR} ->
            [Pid ! {tail, Ref, TR} || {Pid, Ref} <- TWaits],
            stream_ready(H, TR)
    end.

stream_ready(H, TR) ->
    receive
        {ask_head, Ref, Pid} ->
            Pid ! {head, Ref, H},
            stream_ready(H, TR);
        {ask_tail, Ref, Pid} ->
            Pid ! {tail, Ref, TR},
            stream_ready(H, TR)
    end.

the_empty_stream() -> [].

is_empty_stream([]) -> true;
is_empty_stream(_) -> false.

add_stream(S1, S2) ->
    case {is_empty_stream(S1), is_empty_stream(S2)} of
        {true, true} ->
            the_empty_stream();
        {true, false} ->
            S2;
        {false, true} ->
            S1;
        {false, false} ->
            cons_stream(
                head(S1) + head(S2),
                fun(_S) ->
                    add_stream(tail(S1), tail(S2))
                end)
    end.    

%% Terminals
nth_stream(Idx, S) ->
    case is_empty_stream(S) of
        true ->
            throw(no_this_idx);
        false ->
            if
                Idx =:= 1 ->
                    head(S);
                true ->
                    nth_stream(Idx - 1, tail(S))
            end
    end.

test() ->
    Ones = cons_stream(1, fun(S) -> S end),

    Ins = cons_stream(
            1,
            fun(I) ->
                add_stream(I, Ones)
            end),

    Fibs = cons_stream(
            0,
            fun(S1) ->
                cons_stream(
                    1,
                    fun(S2) ->
                        add_stream(S1, S2)
                    end)
            end),

    1 = nth_stream(4, Ones),
    4 = nth_stream(4, Ins),
    2 = nth_stream(4, Fibs),

    test_ok.


