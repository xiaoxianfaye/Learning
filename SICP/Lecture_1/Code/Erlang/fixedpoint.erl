-module(fixedpoint).
-compile(export_all).

-define(TOLERANCE, 0.001).

fixedpoint(F) ->
    fun(Guess) ->
        fun(X) ->
            try_once(Guess, F, X)
        end
    end.

try_once(Guess, F, X) ->
    NextGuess = (F(Guess))(X),
    case good_enough(NextGuess, Guess) of
        true ->
            NextGuess;
        false ->
            try_once(NextGuess, F, X)
    end.

good_enough(NextGuess, Guess) ->
    abs(NextGuess - Guess) < ?TOLERANCE.

test_sqrt() ->
    F = fun(Y) ->
            fun(X) ->
                (Y + X / Y) / 2
            end
        end,
    Sqrt = (fixedpoint(F))(1),
    true = abs(Sqrt(4) - 2) < ?TOLERANCE,
    true = abs(Sqrt(9) - 3) < ?TOLERANCE,
    test_sqrt_ok.

test_cuberoot() ->
    F = fun(Y) ->
            fun(X) ->
                (X / (Y * Y) + 2 * Y) / 3
            end
        end,
    Cuberoot = (fixedpoint(F))(1),
    true = abs(Cuberoot(8) - 2) < ?TOLERANCE,
    true = abs(Cuberoot(27) - 3) < ?TOLERANCE,
    test_cuberoot_ok.

test() ->
    test_sqrt(),
    test_cuberoot(),
    test_ok.