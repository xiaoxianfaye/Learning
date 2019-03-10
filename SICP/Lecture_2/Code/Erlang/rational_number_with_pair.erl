-module(rational_number_with_pair).
-compile(export_all).

-import(pairs, [my_cons/2, my_hd/1, my_tl/1]).

make_rat(N, D) ->
    Gcd = gcd(N, D),
    pairs:my_cons(N div Gcd, D div Gcd).

numer(R) -> pairs:my_hd(R).
denom(R) -> pairs:my_tl(R).

'+rat'(X, Y) ->
    make_rat(numer(X) * denom(Y) + denom(X) * numer(Y), 
             denom(X) * denom(Y)).

'*rat'(X, Y) ->
    make_rat(numer(X) * numer(Y), 
             denom(X) * denom(Y)).

% Euclid's algorithm for greatest common divisor
gcd(X, 0) -> X;
gcd(X, Y) ->
    gcd(Y, X rem Y).

test() ->
    S = '+rat'(make_rat(1, 2), make_rat(1, 4)),
    3 = numer(S),
    4 = denom(S),

    M = '*rat'(make_rat(1, 2), make_rat(2, 3)),
    1 = numer(M),
    3 = denom(M),

    test_ok.