-module(rational_number).
-compile(export_all).

'+rat'(X, Y) ->
    make_rat(numer(X) * denom(Y) + denom(X) * numer(Y), 
             denom(X) * denom(Y)).

'*rat'(X, Y) ->
    make_rat(numer(X) * numer(X), 
             denom(Y) * denom(Y)).

% using_rat() ->
%     '*rat'('+rat'(X, Y),
%            '+rat'(S, T)).

% using_number() ->
%     {Pn, Pd} = '+rat'(Xn, Xd, Yn, Yd),
%     {Qn, Qd} = '+rat'(Sn, Sd, Tn, Td),
%     '*rat'(Pn, Pd, Qn, Qd).

% make_rat(N, D) -> [N|D].

numer(R) -> hd(R).
denom(R) -> tl(R).

% Euclid's algorithm for greatest common divisor
gcd(X, 0) -> X;
gcd(X, Y) ->
    gcd(Y, X rem Y).

make_rat(N, D) ->
    Gcd = gcd(N, D),
    [N div Gcd | D div Gcd].

% another one
% make_rat(N, D) -> [N|D].

% numer([N|D]) ->
%     Gcd = gcd(N, D),
%     N div Gcd.

% denom([N|D]) ->
%     Gcd = gcd(N, D),
%     D div Gcd.

test() ->
    [3|4] = '+rat'(make_rat(2, 4), make_rat(1, 4)),
    test_ok.