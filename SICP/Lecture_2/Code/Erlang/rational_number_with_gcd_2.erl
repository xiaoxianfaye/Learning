-module(rational_number_with_gcd_2).
-compile(export_all).

make_rat(N, D) -> [N|D].

numer([N|D]) ->
    Gcd = gcd(N, D),
    N div Gcd.

denom([N|D]) ->
    Gcd = gcd(N, D),
    D div Gcd.

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