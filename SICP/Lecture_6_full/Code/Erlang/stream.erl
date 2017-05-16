-module(stream).
-compile(export_all).

%% Procedures without Stream
sum_odds_square({Left, Right}) ->
    sum_odds_square(Left) + sum_odds_square(Right);
sum_odds_square(N) ->
    case is_odd(N) of
        true ->
            N * N;
        false ->
            0
    end.

odd_fibs(N) -> odd_fibs(1, N).

odd_fibs(C, N) when C > N -> [];
odd_fibs(C, N) ->
    case is_odd(fib(C)) of
        true ->
            [C | odd_fibs(C + 1, N)];
        false ->
            odd_fibs(C + 1, N)
    end.

is_odd(N) ->
    N rem 2 =/= 0.

fib(N) -> fib(N, 0, 1).

fib(0, A, _) -> A;  
fib(N, A, B) -> fib(N - 1, B, A + B).

square(N) -> N * N.

add(X, Y) -> X + Y.

%% Procedures with Stream
sum_odds_square_s(T) ->
    acc_stream(fun add/2, 0, 
               map_stream(fun square/1, 
                          filter_stream(fun is_odd/1, 
                                        enumerate_tree(T)))).

odd_fibs_s(N) ->
    acc_stream(fun({Idx, _E}, A) -> [Idx|A] end, [], 
               filter_stream(fun is_odd_idx/1, 
                             map_stream(fun fib_idx/1, 
                                        enumerate_interval(1, N)))).

is_odd_idx({_Idx, E}) ->
    is_odd(E).

fib_idx(Idx) ->
    {Idx, fib(Idx)}.

%% Stream
cons_stream(H, T) -> [H|T].
the_empty_stream() -> [].

head(S) -> hd(S).
tail(S) -> tl(S).
is_empty_stream(S) -> S =:= [].

map_stream(Proc, S) ->
    case is_empty_stream(S) of
        true ->
            the_empty_stream();
        false ->
            cons_stream(Proc(head(S)), map_stream(Proc, tail(S)))
    end.

filter_stream(F, S) ->
    case is_empty_stream(S) of
        true ->
            the_empty_stream();
        false ->
            case F(head(S)) of
                true ->
                    cons_stream(head(S), filter_stream(F, tail(S)));
                false ->
                    filter_stream(F, tail(S))
            end
    end.

acc_stream(Proc, A, S) ->
    case is_empty_stream(S) of
        true ->
            A;
        false ->
            Proc(head(S), acc_stream(Proc, A, tail(S)))
    end.

acc_stream_2(Proc, A, S) ->
    case is_empty_stream(S) of
        true ->
            A;
        false ->
            acc_stream_2(Proc, Proc(head(S), A), tail(S))
    end.

append_stream(S1, S2) ->
    case is_empty_stream(S1) of
        true ->
            S2;
        false ->
            cons_stream(head(S1), append_stream(tail(S1), S2))
    end.

enumerate_tree({Left, Right}) ->
    append_stream(enumerate_tree(Left), enumerate_tree(Right));
enumerate_tree(N) ->
    cons_stream(N, the_empty_stream()).

enumerate_interval(Low, High) when Low > High ->
    the_empty_stream();
enumerate_interval(Low, High) ->
    cons_stream(Low, enumerate_interval(Low + 1, High)).

flatten(StOfSt) ->
    acc_stream(fun append_stream/2, the_empty_stream(), StOfSt).

flatmap(P, S) ->
    flatten(map_stream(P, S)).

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

test_sum_odds_square() ->
    411 = sum_odds_square({{1, {2, 7}}, {19, {12, 14}}}),
    test_sum_odds_square_ok.
    
test_odd_fibs() ->
    [1, 2, 4, 5] = odd_fibs(6),
    test_odd_fibs_ok.

test_map_stream() ->
    [2, 4] = map_stream(fun(X) -> X * 2 end, [1, 2]),
    test_map_stream_ok.

test_filter_stream() ->
    [2, 4] = filter_stream(fun(X) -> X rem 2 =:= 0 end, [1, 2, 3, 4]),
    test_filter_stream_ok.

test_acc_stream() ->
    10 = acc_stream(fun(X, Y) -> X + Y end, 0, [1, 2, 3, 4]),
    10 = acc_stream_2(fun(X, Y) -> X + Y end, 0, [1, 2, 3, 4]),
    test_acc_stream_ok.

test_append_stream() ->
    [1, 2, 3, 4] = append_stream([1, 2], [3, 4]),
    test_append_stream_ok.

test_enumerate_tree() ->
    [1, 2, 7, 19, 12, 14] = enumerate_tree({{1, {2, 7}}, {19, {12, 14}}}),
    test_enumerate_tree_ok.

test_enumerate_interval() ->
    [1, 2, 3, 4] = enumerate_interval(1, 4),
    test_enumerate_interval_ok.

test_sum_odds_square_s() -> 
    411 = sum_odds_square_s({{1, {2, 7}}, {19, {12, 14}}}),
    test_sum_odds_square_s_ok.

test_odd_fibs_s() ->
    [1, 2, 4, 5] = odd_fibs_s(6),
    test_odd_fibs_s_ok.

test_flatten() ->
    [1, 2, 3, 4, 5] = flatten([[1, 2], [3, 4, 5]]),
    test_flatten_ok.

test_flatmap() ->
    [1, 2, 1, 2, 3] = flatmap(fun(X) -> lists:seq(1, X) end, [2, 3]),
    test_flatmap_ok.

test_is_prime() ->
    true = is_prime(2),
    true = is_prime(3),
    false = is_prime(4),
    true = is_prime(5),
    test_is_prime_ok.

test_prime_sum_pairs() ->
    [{2,1,3}, {3,2,5}, {4,1,5}, {4,3,7}, {5,2,7}] = prime_sum_pairs(5),
    test_prime_sum_pairs_ok.

test_triples() ->
    [{3,2,1}, 
     {4,2,1}, {4,3,1}, {4,3,2},
     {5,2,1}, {5,3,1}, {5,3,2}, {5,4,1}, {5,4,2}, {5,4,3}] = triples(5),
     test_triples_ok.    

test() ->
    test_sum_odds_square(),
    test_odd_fibs(),

    test_map_stream(),
    test_filter_stream(),
    test_acc_stream(),
    test_append_stream(),

    test_enumerate_tree(),
    test_enumerate_interval(),

    test_sum_odds_square_s(),
    test_odd_fibs_s(),

    test_flatten(),
    test_flatmap(),

    test_is_prime(),
    test_prime_sum_pairs(),
    test_triples(),

    test_ok.