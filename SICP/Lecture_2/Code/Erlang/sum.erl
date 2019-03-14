-module(sum).
-compile(export_all).

sum_int(A, B) when A > B -> 0;
sum_int(A, B) -> A + sum_int(A + 1, B).

sum_int_i(A, B) ->
    sum_int_i(A, B, 0).
sum_int_i(A, B, Acc) when A > B ->
    Acc;
sum_int_i(A, B, Acc) ->
    sum_int_i(A + 1, B, Acc + A).

sum_sq(A, B) when A > B -> 0;
sum_sq(A, B) -> A * A + sum_sq(A + 1, B).

sum_sq_i(A, B) ->
    sum_sq_i(A, B, 0).
sum_sq_i(A, B, Acc) when A > B ->
    Acc;
sum_sq_i(A, B, Acc) ->
    sum_sq_i(A + 1, B, Acc + A * A).

pi_sum(A, B) when A > B -> 0;
pi_sum(A, B) ->    1 / (A * (A + 2)) + pi_sum(A + 4, B).

pi_sum_i(A, B) ->
    pi_sum_i(A, B, 0).
pi_sum_i(A, B, Acc) when A > B ->
    Acc;
pi_sum_i(A, B, Acc) ->
    pi_sum_i(A + 4, B, Acc + 1 / (A * (A + 2))).

sum(_Term, A, _Next, B) when A > B -> 0;
sum(Term, A, Next, B) ->
    Term(A) + sum(Term, Next(A), Next, B).

sum_i(Term, A, Next, B) ->
    sum_i(Term, A, Next, B, 0).
sum_i(_Term, A, _Next, B, Acc) when A > B ->
    Acc;
sum_i(Term, A, Next, B, Acc) ->
    sum_i(Term, Next(A), Next, B, Acc + Term(A)).

sum_int_2(A, B) ->
    sum(fun(X) -> X end, A, fun(X) -> X + 1 end, B).

sum_int_3(A, B) ->
    sum_i(fun(X) -> X end, A, fun(X) -> X + 1 end, B).

sum_sq_2(A, B) ->
    sum(fun(X) -> X * X end, A, fun(X) -> X + 1 end, B).

sum_sq_3(A, B) ->
    sum_i(fun(X) -> X * X end, A, fun(X) -> X + 1 end, B).

pi_sum_2(A, B) ->
    sum(fun(X) -> 1 / (X * (X + 2)) end, A, fun(X) -> X + 4 end, B).

pi_sum_3(A, B) ->
    sum_i(fun(X) -> 1 / (X * (X + 2)) end, A, fun(X) -> X + 4 end, B).

test() ->
    55 = sum_int(1, 10),
    55 = sum_int_i(1, 10),
    30 = sum_sq(1, 4),
    30 = sum_sq_i(1, 4),
    0.372005772005772 = pi_sum(1, 10),
    0.372005772005772 = pi_sum_i(1, 10),
    55 = sum_int_2(1, 10),
    55 = sum_int_3(1, 10),
    30 = sum_sq_2(1, 4),
    30 = sum_sq_3(1, 4),
    0.372005772005772 = pi_sum_2(1, 10),
    0.372005772005772 = pi_sum_3(1, 10),
    test_ok.