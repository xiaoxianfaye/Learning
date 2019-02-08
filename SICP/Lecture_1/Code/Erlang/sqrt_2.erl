-module(sqrt_2).
-compile(export_all).

-define(TOLERANCE, 0.001).

sqrt(X) ->
    my_try(1, X).

my_try(Guess, X) ->
    NextGuess = average(Guess, X / Guess),
    case good_enough(NextGuess, Guess) of
        true ->
            NextGuess;
        false ->
            my_try(NextGuess, X)
    end.

good_enough(NextGuess, Guess) ->
    abs(NextGuess - Guess) < ?TOLERANCE.

average(X, Y) ->
    (X + Y) / 2.

test() ->
    true = abs(sqrt(4) - 2) < ?TOLERANCE,
    true = abs(sqrt(9) - 3) < ?TOLERANCE,
    test_ok.
