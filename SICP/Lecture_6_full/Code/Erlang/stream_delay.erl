-module(stream_delay).
-compile(export_all).

-type stream() :: {term(), fun(() -> stream())}.

%% Stream
-spec cons_stream(term(), fun(() -> stream())) -> stream().
cons_stream(H, T) -> [H, T].
head([H, _]) -> H.
tail([_, T]) -> T().

the_empty_stream() -> [].

is_empty_stream([]) -> true;
is_empty_stream(_) -> false.

map_stream(Proc, S) ->
    case is_empty_stream(S) of
        true ->
            the_empty_stream();
        false ->
            cons_stream(Proc(head(S)), 
                        fun() ->
                            map_stream(Proc, tail(S))
                        end)
    end.

filter_stream(F, S) ->
    case is_empty_stream(S) of
        true ->
            the_empty_stream();
        false ->
            case F(head(S)) of
                true ->
                    cons_stream(head(S), 
                                fun() ->
                                    filter_stream(F, tail(S))
                                end);
                false ->
                    filter_stream(F, tail(S))
            end
    end.

append_stream(S1, S2) ->
    case is_empty_stream(S1) of
        true ->
            S2;
        false ->
            cons_stream(head(S1), 
                        fun() ->
                            append_stream(tail(S1), S2)
                        end)
    end.

enumerate_tree({Left, Right}) ->
    append_stream(enumerate_tree(Left), enumerate_tree(Right));
enumerate_tree(N) ->
    cons_stream(N, 
                fun() ->
                    the_empty_stream()
                end).

enumerate_interval(Low, High) when Low > High ->
    the_empty_stream();
enumerate_interval(Low, High) ->
    cons_stream(Low, 
                fun() ->
                    enumerate_interval(Low + 1, High)
                end).

acc_stream(Proc, A, S) ->
    case is_empty_stream(S) of
        true ->
            A;
        false ->
            Proc(head(S), acc_stream(Proc, A, tail(S)))
    end.

flatten(StOfSt) ->
    acc_stream(fun append_stream/2, the_empty_stream(), StOfSt).

flatmap(P, S) ->
    flatten(map_stream(P, S)).

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
                fun() ->
                    add_stream(tail(S1), tail(S2))
                end)
    end.

scale(C, S) ->
    map_stream(fun(E) -> C * E end, S).

%% Stream Generators
integers(N) -> integers(1, N).

integers(Low, High) -> integers_inc(Low, High, 1).

integers_inc(Low, High, Inc) ->
    cons_stream(Low,
                fun() ->
                    if
                        Low < High ->
                            integers_inc(Low + Inc, High, Inc);
                        true ->
                            the_empty_stream()
                    end
                end).

list_to_stream([]) ->
    the_empty_stream();
list_to_stream([H|T]) ->
    cons_stream(H, 
                fun() ->
                    list_to_stream(T)
                end).

%% Terminals
collect_stream(S) ->
    case is_empty_stream(S) of
        true -> [];
        false -> [head(S)|collect_stream(tail(S))]
    end.

collect_stream_limit(0, _S) -> [];
collect_stream_limit(Num, S) ->
    [head(S)|collect_stream_limit(Num - 1, tail(S))].

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

print_stream(S) ->
    case is_empty_stream(S) of
        true ->
            io:format("Done~n");
        false ->
            io:format("~p~n", [head(S)]),
            print_stream(tail(S))
    end.

print_stream_limit(0, _S) ->
    io:format("Done limited~n");
print_stream_limit(N, S) ->
    case is_empty_stream(S) of
        true ->
            io:format("Done completely~n");
        false ->
            io:format("~p~n", [head(S)]),
            print_stream_limit(N - 1, tail(S))
    end.

%% Procedures with Stream
sum_odds_square(T) ->
    acc_stream(fun add/2, 0, 
               map_stream(fun square/1, 
                          filter_stream(fun is_odd/1, 
                                        enumerate_tree(T)))).

odd_fibs(N) ->
    acc_stream(fun({Idx, _E}, A) -> [Idx|A] end, [], 
               filter_stream(fun is_odd_idx/1, 
                             map_stream(fun fib_idx/1, 
                                        enumerate_interval(1, N)))).

is_odd(N) ->
    N rem 2 =/= 0.

fib(N) -> fib(N, 0, 1).

fib(0, A, _) -> A;  
fib(N, A, B) -> fib(N - 1, B, A + B).

square(N) -> N * N.

add(X, Y) -> X + Y.

is_odd_idx({_Idx, E}) ->
    is_odd(E).

fib_idx(Idx) ->
    {Idx, fib(Idx)}.

%% I J Pair Prime with Stream
prime_sum_pairs(N) ->
    filter_stream(
        fun({_, _, S}) -> 
            is_prime(S)
        end, 
        flatmap(
            fun(I) -> 
                map_stream(
                    fun(J) -> 
                        {I, J, I + J} 
                    end, 
                    enumerate_interval(1, I - 1)) 
            end, 
            enumerate_interval(1, N))).

is_prime(N) -> is_prime(2, N).

is_prime(I, N) when I >= N -> 
    true;
is_prime(I, N) ->
    if
        N rem I =:= 0 ->
            false;
        true ->
            is_prime(I + 1, N)
    end.

%% Embedded Flatmap
triples(N) ->
    flatmap(
        fun(I) -> 
            flatmap(
                fun(J) -> 
                    map_stream(fun(K) -> {I, J, K} end, enumerate_interval(1, J - 1)) 
                end, enumerate_interval(1, I - 1)) 
        end, enumerate_interval(1, N)).

%% Second Prime
second_prime() ->
    head(
        tail(
            filter_stream(
                fun is_prime/1,
                enumerate_interval(10000, 1000000)))).    

%% Infinite Stream
integers_from(N) ->
    cons_stream(N, fun() -> integers_from(N + 1) end). 

-define(INT, integers_from(1)).    

no_sevens() ->
    filter_stream(
        fun(N) ->
            N rem 7 =/= 0
        end, ?INT).

%% Sieve of Eratosthenes
primes() ->
    sieve(integers_from(2)).

sieve(S) ->
    cons_stream(head(S),
                fun() ->
                    sieve(
                        filter_stream(
                            fun(N) ->
                                N rem head(S) =/= 0
                            end,
                            tail(S)))
                end).

%% Defining streams implicitly
ones() ->
    % cons_stream(1, fun() -> ones() end).
    cons_stream(1, fun ones/0).

integers() ->
    cons_stream(1, 
                fun() ->
                    add_stream(integers(), ones())
                end).

fibs(N1, N2) ->
    cons_stream(N1,
                fun() ->
                    fibs(N2, N1 + N2)
                end).

fibs() ->
    cons_stream(0,
                fun() ->
                    cons_stream(1,
                                fun() ->
                                    add_stream(fibs(), tail(fibs()))
                                end)
                end).

%% Integrator
integral(S, Init, Dt) ->
    cons_stream(Init,
                fun() ->
                    add_stream(scale(Dt, S), integral(S, Init, Dt))
                end).

funmaps(Fun, Init, Dt) ->
    cons_stream(Fun(Init),
                fun() ->
                    funmaps(Fun, Init + Dt, Dt)
                end).

integrator(Fun, Init, Dt) ->
    Ones = ones(),
    S = map_stream(Fun, integral(Ones, Init, Dt)),
    integral(S, 0, Dt).

%% Differential Equation
% ys() ->
%     integral(dys(), 1, 0.001).
    
% dys() ->
%     map_stream(fun(E) -> E * E end, ys()).

integral_delay(DelayS, Init, Dt) ->
    cons_stream(Init,
                fun() ->
                    S = DelayS(),
                    add_stream(scale(Dt, S), integral(S, Init, Dt))
                end).

ys() ->
    integral_delay(fun() -> dys() end, 1, 0.001).

dys() ->
    map_stream(fun(E) -> E * E end, ys()).

%% Tests
test_map_stream() ->
    [2, 4] = collect_stream(
                map_stream(fun(X) -> X * 2 end, list_to_stream([1, 2]))),
    test_map_stream_ok.

test_filter_stream() ->
    [2, 4] = collect_stream(
                filter_stream(fun(X) -> X rem 2 =:= 0 end, 
                              list_to_stream([1, 2, 3, 4]))),
    test_filter_stream_ok.

test_append_stream() ->
    [1, 2, 3, 4] = collect_stream(
                        append_stream(list_to_stream([1, 2]), 
                                      list_to_stream([3, 4]))),
    test_append_stream_ok.

test_enumerate_tree() ->
    [1, 2, 7, 19, 12, 14] = collect_stream(
                                enumerate_tree({{1, {2, 7}}, {19, {12, 14}}})),
    test_enumerate_tree_ok.

test_enumerate_interval() ->
    [1, 2, 3, 4] = collect_stream(enumerate_interval(1, 4)),
    test_enumerate_interval_ok.

test_acc_stream() ->
    10 = acc_stream(fun(X, Y) -> X + Y end, 0, list_to_stream([1, 2, 3, 4])),
    test_acc_stream_ok.

test_flatten() ->
    [1, 2, 3, 4, 5] = collect_stream(
                        flatten(list_to_stream([list_to_stream([1, 2]), 
                                                list_to_stream([3, 4, 5])]))),
    test_flatten_ok.

test_flatmap() ->
    [1, 2, 1, 2, 3] = collect_stream(
                        flatmap(fun(X) -> list_to_stream(lists:seq(1, X)) end, 
                                list_to_stream([2, 3]))),
    test_flatmap_ok.

test_add_stream() ->
    [4, 6] = collect_stream(add_stream(integers(2), integers(3, 4))).

test_scale() ->
    [2, 4] = collect_stream(scale(2, integers(2))).    

test_nth_stream() ->
    S = list_to_stream([1, 2]),
    1 = nth_stream(1, S),
    2 = nth_stream(2, S),
    test_nth_stream_ok.

test_print_stream() ->
    S = list_to_stream([1, 2]),
    print_stream(S).

test_sum_odds_square() -> 
    411 = sum_odds_square({{1, {2, 7}}, {19, {12, 14}}}),
    test_sum_odds_square_ok.

test_odd_fibs() ->
    [1, 2, 4, 5] = odd_fibs(6),
    test_odd_fibs_ok.

test_prime_sum_pairs() ->
    [{2,1,3}, {3,2,5}, {4,1,5}, {4,3,7}, {5,2,7}] = 
        collect_stream(prime_sum_pairs(5)),
    test_prime_sum_pairs_ok.

test_triples() ->
    [{3,2,1}, 
     {4,2,1}, {4,3,1}, {4,3,2},
     {5,2,1}, {5,3,1}, {5,3,2}, {5,4,1}, {5,4,2}, {5,4,3}] = collect_stream(triples(5)),
     test_triples_ok.

test_second_prime() ->
    10009 = second_prime(),
    test_second_prime_ok.

test_integers_from() ->
    S = integers_from(2),
    2 = nth_stream(1, S),
    3 = nth_stream(2, S),

    1 = nth_stream(1, ?INT),
    2 = nth_stream(2, ?INT),

    test_integers_from_ok.

test_no_sevens() ->
    S = no_sevens(),
    1 = nth_stream(1, S),
    8 = nth_stream(7, S),
    16 = nth_stream(14, S),

    test_no_sevens_ok.

test_primes() ->
    Ps = primes(),
    2 = nth_stream(1, Ps),
    3 = nth_stream(2, Ps),
    5 = nth_stream(3, Ps),
    23 = nth_stream(9, Ps),
    29 = nth_stream(10, Ps),
    71 = nth_stream(20, Ps),

    Ps2 = primes(),
    [2, 3, 5, 7, 11, 13, 17, 19, 23, 29] = collect_stream_limit(10, Ps2),
    
    test_primes_ok.

test_ones() ->
    [1, 1, 1, 1, 1] = collect_stream_limit(5, ones()),
    test_ones_ok.

test_integers() ->
    [1, 2, 3, 4, 5] = collect_stream_limit(5, integers()),
    test_integers_ok.

test_fibs() ->
    [0, 1, 1, 2, 3, 5] = collect_stream_limit(6, fibs(0, 1)),
    [0, 1, 1, 2, 3, 5] = collect_stream_limit(6, fibs()),
    test_fibs_ok.

test_integral() ->
    [0, 1, 2, 3, 4] = collect_stream_limit(5, integral(ones(), 0, 1)),
    test_integral_ok.

test_funmaps() ->
    [0, 1, 2, 3, 4] = collect_stream_limit(5, funmaps(fun(X) -> X end, 0, 1)),
    test_funmaps_ok.

test_funmaps_integral() ->
    IdFs = funmaps(fun(X) -> X end, 1, 0.001),
    S1 = integral(IdFs, 0, 0.001),
    io:format("~p~n", [nth_stream(1001, S1)]),

    SqFs = funmaps(fun(X) -> X * X end, 1, 0.001),
    S2 = integral(SqFs, 0, 0.001),
    io:format("~p~n", [nth_stream(1001, S2)]),

    SqFs2 = funmaps(fun(X) -> X * X end, 0, 0.001),
    S3 = integral(SqFs2, 0, 0.001),
    io:format("~p~n", [nth_stream(1001, S3)]).

test_integrator() ->
    S1 = integrator(fun(X) -> X end, 1, 0.01),
    io:format("~p~n", [nth_stream(101, S1)]),

    S2 = integrator(fun(X) -> X * X end, 1, 0.01),
    io:format("~p~n", [nth_stream(101, S2)]),

    S3 = integrator(fun(X) -> X * X end, 0, 0.01),
    io:format("~p~n", [nth_stream(101, S3)]).

test() ->
    test_map_stream(),
    test_filter_stream(),
    test_append_stream(),

    test_enumerate_tree(),
    test_enumerate_interval(),

    test_acc_stream(),
    test_flatten(),
    test_flatmap(),
    test_add_stream(),
    test_scale(),

    test_nth_stream(),

    test_sum_odds_square(),
    test_odd_fibs(),
    test_prime_sum_pairs(),
    test_triples(),

    test_second_prime(),

    test_integers_from(),
    test_no_sevens(),
    test_primes(),

    test_ones(),
    test_integers(),
    test_fibs(),

    test_integral(),
    test_funmaps(),

    test_ok.