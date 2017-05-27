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

%% Cesaro
-define(RANGE, 100000).

rand_stream() ->
    {Rd, NSeed} = random:uniform_s(?RANGE, erlang:now()),
    cons_stream(
        {Rd, NSeed},
        fun(RS) ->
            map_stream(
                fun({_, Seed}) ->
                    random:uniform_s(?RANGE, Seed)
                end,
                RS)
        end).

map_stream(Proc, S) ->
    case is_empty_stream(S) of
        true ->
            the_empty_stream();
        false ->
            cons_stream(
                Proc(head(S)), 
                fun(_) ->
                    map_stream(Proc, tail(S))
                end)
    end.    

map_successive_pairs(F, S) ->
    cons_stream(
        F(head(S), head(tail(S))),
        fun(_) ->
            map_successive_pairs(
                F,
                tail(tail(S)))
        end).

gcd(X, 0) -> X;
gcd(X, Y) -> gcd(Y, X rem Y).

cesaro_stream(Rs) ->
    map_successive_pairs(
        fun({R1, _}, {R2, _}) ->
            gcd(R1, R2) =:= 1
        end, Rs).

monte_carlo(S, Total, Passed) ->
    NPassed = case head(S) of
                true ->
                    Passed + 1;
                false ->
                    Passed
              end,
    cons_stream(
        NPassed / (Total + 1),
        fun(_) ->
            monte_carlo(tail(S), Total + 1, NPassed)
        end).

pis() ->
    map_stream(
        fun(0.0) -> 0.0;
           (P) -> math:sqrt(6 / P)
        end,
        monte_carlo(cesaro_stream(rand_stream()), 0, 0)).

pi(Toler) ->
    stream_limit(pis(), Toler).

stream_limit(S, Toler) ->
    stream_limit(head(S), tail(S), Toler).

stream_limit(Pre, S, Toler) ->
    Next = head(S),
    if
        abs(Pre - Next) =< Toler ->
            Next;
        true ->
            stream_limit(Next, tail(S), Toler)
    end.

%% A functional-programming view of time
stream_withdraw(Balance, AmountS) ->
    cons_stream(
        Balance,
        fun(_) ->
            stream_withdraw(
                Balance - head(AmountS),
                tail(AmountS))
        end).
