-module(sqrt_2).
-compile(export_all).

-define(TOLERANCE, 0.001).

sqrt(X) ->
    try_once(1, X).

try_once(Guess, X) ->
    NextGuess = average(Guess, X / Guess),
    case good_enough(NextGuess, Guess) of
        true ->
            NextGuess;
        false ->
            try_once(NextGuess, X)
    end.

good_enough(NextGuess, Guess) ->
    abs(NextGuess - Guess) < ?TOLERANCE.

average(X, Y) ->
    (X + Y) / 2.

test() ->
    true = abs(sqrt(4) - 2) < ?TOLERANCE,
    true = abs(sqrt(9) - 3) < ?TOLERANCE,
    test_ok.