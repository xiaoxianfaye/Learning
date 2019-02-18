-module(hanoi).
-compile(export_all).

% Recursive Version: output io:format
hanoi_r(0, _, _, _) -> 
    done;
hanoi_r(N, From, To, Spare) ->
    hanoi_r(N - 1, From, Spare, To),
    io:format("~p: ~p -> ~p~n", [N, From, To]),
    hanoi_r(N - 1, Spare, To, From).

% Recursive Version: output a sequence 
hanoi_r_2(N, From, To, Spare) ->
    hanoi_r_2(N, From, To, Spare, []).

hanoi_r_2(0, _From, _To, _Spare, Acc) -> 
    Acc;
hanoi_r_2(N, From, To, Spare, Acc) ->
    NewAcc1 = hanoi_r_2(N - 1, From, Spare, To, Acc),
    NewAcc2 = move_r_2(N, From, To, NewAcc1),
    hanoi_r_2(N - 1, Spare, To, From, NewAcc2).

move_r_2(N, From, To, Acc) ->
    Acc ++ [{N, From, To}].

% Iterative Version SunMing
hanoi_i(N, From, To, Spare) ->
    InitSteps = init_steps(N, From, To, Spare, []),
    InitSeq = lists:zip([{power2(T - 1), T, power2(T)} || T <- lists:seq(1, N)], InitSteps),
    generate_seq(InitSeq, power2(N) - 1, 1, []).

init_steps(0, _From, _To, _Spare, Acc) ->
    Acc;
init_steps(N, From, To, Spare, Acc) ->
    init_steps(N - 1, From, Spare, To, [{From, To, Spare} | Acc]).

generate_seq(Seq, Total, Cur, Acc) ->
    {No, From, To, NewSeq} = get_cur(Cur, Seq),
    NewAcc = move(No, From, To, Acc),
    if
        Cur =:= Total ->
            NewAcc;
        true ->
            generate_seq(NewSeq, Total, Cur + 1, NewAcc)
    end.

get_cur(Cur, [{{Cur, No, Step}, {From, To, Spare}} | T]) ->
    {No, From, To, [{{Cur + Step, No, Step}, {To, Spare, From}} | T]};
get_cur(Cur, [H|T]) ->
    {No, From, To, New} = get_cur(Cur, T),
    {No, From, To, [H|New]}.

move(No, From, To, Acc) ->
    Acc ++ [{No, From, To}].

power2(0) -> 1;
power2(N) -> 2 * power2(N - 1).

% Iterative Version Faye
hanoi_i_2(N, From, To, Spare) ->
    InitSteps = init_steps_2(N, From, To, Spare, []),
    InitSeq = init_seq_2(N, InitSteps, []),
    generate_seq_2(InitSeq, power2(N) - 1, 1, []).

init_steps_2(0, _From, _To, _Spare, Acc) ->
    lists:reverse(Acc);
init_steps_2(N, From, To, Spare, Acc) ->
    init_steps_2(N - 1, From, Spare, To, [{From, To, Spare} | Acc]).

init_seq_2(0, [], Acc) ->
    Acc;
init_seq_2(N, [{From, To, Spare}|T], Acc) ->
    init_seq_2(N - 1, T, [{power2(N - 1), {N, power2(N), From, To, Spare}} | Acc]).

generate_seq_2(Seq, Total, Cur, Acc) ->
    {No, From, To, NewSeq} = get_cur_2(Cur, Seq),
    NewAcc = move(No, From, To, Acc),
    if
        Cur =:= Total ->
            NewAcc;
        true ->
            generate_seq_2(NewSeq, Total, Cur + 1, NewAcc)
    end.

get_cur_2(Cur, Seq) ->
    case lists:keyfind(Cur, 1, Seq) of
        {Cur, {No, Step, From, To, Spare}} ->
            NewSeq = lists:keydelete(Cur, 1, Seq),
            {No, From, To, [{Cur + Step, {No, Step, To, Spare, From}} | NewSeq]};
        false ->
            error
    end.

% Iterative Version Const
hanoi_i_3(N, From, To, Spare) ->
    hanoi_const(power2(N) - 1, 1, even_odd_mover(N, From, To, Spare), []).

hanoi_const(Total, Cur, {EvenMover, OddMover}=Gen, Acc) ->
    {SpikeNo, SubCircularStep} = spike_substep(Cur),
    io:format("Cur ~p SpikeNo ~p SubCircularStep ~p~n", [Cur, SpikeNo, SubCircularStep]),
    {From, To} = 
        if
            SpikeNo rem 2 =:= 0 ->
                EvenMover(SubCircularStep);
            true ->
                OddMover(SubCircularStep) 
        end,
    Acc1 = move(SpikeNo, From, To, Acc),
    if
        Total =:= Cur ->
            Acc1;
        true ->
            hanoi_const(Total, Cur + 1, Gen, Acc1) 
    end.

even_odd_mover(N, From, To, Spare) when N rem 2 =:= 0 ->
    {gen_mover(From, To, Spare), gen_mover(From, Spare, To)};
even_odd_mover(_N, From, To, Spare) ->
    {gen_mover(From, Spare, To), gen_mover(From, To, Spare)}.

spike_substep(Cur) -> spike_substep(Cur, 0).

spike_substep(Cur, N) when Cur rem 2 =/= 0 ->
    {N + 1, ((Cur + 1) div 2) rem 3};
spike_substep(Cur, N) ->
    spike_substep(Cur div 2, N + 1).

gen_mover(From, Spare, To) ->
    fun(1) ->
        {From, To};
       (2) ->
        {To, Spare};
       (0) ->
        {Spare, From}
    end.