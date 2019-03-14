-module(fixed_points).
-compile(export_all).

-define(TOLERANCE, 0.00001).
-define(DELTA_X, 0.00001).

fixed_points(Fun, Guess) ->
    fixed_points_iter(Fun, Guess, Fun(Guess)).

fixed_points_iter(Fun, Old, New) ->
    case close_enough(Old, New) of
        true ->
            New;
        false ->
            fixed_points_iter(Fun, New, Fun(New))
    end.

close_enough(Old, New) ->
    abs(New - Old) < ?TOLERANCE.

average(X, Y) ->
    (X + Y) / 2.

average_damp(F) ->
    fun(Y) ->
        average(Y, F(Y))
    end.

newton(F, Guess) ->
    newton(F, deriv(F), Guess).

newton(F, Df, Guess) ->
    fixed_points(fun(Y) -> Y - F(Y) / Df(Y) end, Guess).

deriv(F) ->
    fun(X) ->
        (F(X + ?DELTA_X) - F(X)) / ?DELTA_X
    end.

square(X) ->
    X * X.

sqrt(X) ->
    fixed_points(fun(Y) -> average(Y, X / Y) end, 1).

sqrt_2(X) ->
    fixed_points(average_damp(fun(Y) -> X / Y end), 1).

sqrt_3(X) ->
    newton(fun(Y) -> X - square(Y) end, 1).

test() ->
    true = abs(sqrt(4) - 2) < ?TOLERANCE,
    true = abs(sqrt(9) - 3) < ?TOLERANCE,
    true = abs(sqrt_2(4) - 2) < ?TOLERANCE,
    true = abs(sqrt_2(9) - 3) < ?TOLERANCE,
    true = abs(sqrt_3(4) - 2) < ?TOLERANCE,
    true = abs(sqrt_3(9) - 3) < ?TOLERANCE,
    test_ok.