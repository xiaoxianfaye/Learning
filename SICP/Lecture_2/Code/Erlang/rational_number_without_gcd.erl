-module(rational_number_without_gcd).
-compile(export_all).

make_rat(N, D) -> [N|D].

numer(R) -> hd(R).
denom(R) -> tl(R).

'+rat'(X, Y) ->
    make_rat(numer(X) * denom(Y) + denom(X) * numer(Y), 
             denom(X) * denom(Y)).

'*rat'(X, Y) ->
    make_rat(numer(X) * numer(Y), 
             denom(X) * denom(Y)).

% using_rat() ->
%     '*rat'('+rat'(X, Y),
%            '+rat'(S, T)).

% using_number() ->
%     {Pn, Pd} = '+rat'(Xn, Xd, Yn, Yd),
%     {Qn, Qd} = '+rat'(Sn, Sd, Tn, Td),
%     '*rat'(Pn, Pd, Qn, Qd).

test() ->
    S = '+rat'(make_rat(1, 2), make_rat(1, 4)),
    6 = numer(S),
    8 = denom(S),

    M = '*rat'(make_rat(1, 2), make_rat(2, 3)),
    2 = numer(M),
    6 = denom(M),

    test_ok.