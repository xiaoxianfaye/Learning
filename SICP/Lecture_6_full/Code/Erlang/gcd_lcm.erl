-module(gcd_lcm).
-compile(export_all).

%% prime factorization
prime_factorization(N) ->
    prime_factorization(N, 2, []).

prime_factorization(1, _C, PFPs) ->
    lists:reverse(PFPs);
prime_factorization(N, C, PFPs) ->
    case is_prime(C) andalso N rem C =:= 0 of
        true ->
            NewPFPs = acc_prime_factors(C, PFPs),
            prime_factorization(N div C, 2, NewPFPs);
        false ->
            prime_factorization(N, C + 1, PFPs)
    end.

acc_prime_factors(PFP, PFPs) ->
    case lists:keyfind(PFP, 1, PFPs) of
        false ->
            [{PFP, 1} | PFPs];
        {PF, Power} ->
            lists:keyreplace(PFP, 1, PFPs, {PF, Power + 1})
    end.

is_prime(N) -> is_prime(2, N).

is_prime(I, N) when I =:= N -> 
    true;
is_prime(I, N) ->
    if
        N rem I =:= 0 ->
            false;
        true ->
            is_prime(I + 1, N)
    end.

%% Set
intersect(Ss) ->
    reduce(fun intersect/2, Ss).

union(Ss) ->
    reduce(fun union/2, Ss).

reduce(_F, []) ->
    [];
reduce(_F, [S]) ->
    S;
reduce(F, [H1, H2|T]) ->
    reduce(F, [F(H1, H2)|T]).

intersect([], _S2) ->
    [];
intersect([H1|T1], S2) ->
    case lists:member(H1, S2) of
        true ->
            [H1|intersect(T1, S2)];
        false ->
            intersect(T1, S2)
    end.

union([], S2) ->
    lists:sort(lists:reverse(S2));
union([H1|T1], S2) ->
    case lists:member(H1, S2) of
        false ->
            union(T1, [H1|S2]);
        true ->
            union(T1, S2)
    end.

%% Greatest Common Divisor
gcd(Ns) ->
    ListOfPFPs = [prime_factorization(N) || N <- Ns],
    ListOfPFsAndPs = [lists:unzip(PFPs) || PFPs <- ListOfPFPs],
    {ListOfPFs, _} = lists:unzip(ListOfPFsAndPs),
    CPFs = intersect(ListOfPFs),
    CPFPs = common_primefactor_powers(CPFs, ListOfPFPs),
    CPFPs.

common_primefactor_powers(CPFs, ListOfPFPs) ->
    



%% Tests
test_is_prime() ->
    true = is_prime(2),
    true = is_prime(3),
    false = is_prime(4),
    true = is_prime(5),
    test_is_prime_ok.

test_prime_factorization() ->
    [{2, 1}] = prime_factorization(2),
    [{2, 1}, {3, 1}] = prime_factorization(6),

    [{2, 1}, {3, 2}, {5, 1}] = prime_factorization(90),
    [{2, 2}, {3, 1}, {5, 1}, {7, 1}] = prime_factorization(420),
    [{2, 1}, {3, 3}, {5, 2}, {7, 1}] = prime_factorization(9450),

    test_prime_factorization_ok.

test_intersect() ->
    [] = intersect([], []),
    [] = intersect([1], []),
    [] = intersect([], [1]),
    [2, 3] = intersect([1, 2, 3, 5], [2, 3, 4]),
    [] = intersect([1, 2, 3], [4, 5]),

    [] = intersect([]),
    [1] = intersect([[1]]),
    [2, 3] = intersect([[1, 2, 3, 5], [2, 3, 4]]),
    [] = intersect([[1, 2, 3], [4, 5]]),
    [3, 4] = intersect([[1, 2, 3, 4], [3, 4, 5], [3, 4, 5, 6]]),

    test_intersect_ok.

test_union() ->
    [] = union([], []),
    [1] = union([1], []),
    [1] = union([], [1]),
    [1, 2, 3, 4] = union([1, 3], [2, 4]),
    [1, 2, 3, 4] = union([1, 2, 3], [3, 4]),
    [1, 2, 3, 4, 5] = union([1, 2, 3], [2, 3, 4, 5]),

    [] = union([]),
    [1] = union([[1]]),
    [1, 2, 3, 4] = union([[1, 3], [2, 4]]),
    [1, 2, 3, 4] = union([[1, 2, 3], [3, 4]]),
    [1, 2, 3, 4, 5] = union([[1, 2, 3], [2, 3, 4, 5]]),
    [1, 2, 3, 4, 5, 6] = union([[1, 2, 3], [3, 4], [4, 5, 6]]),

    test_union_ok.

test() ->
    test_is_prime(),
    test_prime_factorization(),

    test_intersect(),
    test_union(),

    test_ok.