-module(sqrt).
-compile(export_all).

-define(TOLERANCE, 0.001).

sqrt(X) -> 
    try_once(1, X).

try_once(Guess, X) ->
    case good_enough(Guess, X) of
        true ->
            Guess;
        false ->
            try_once(improve(Guess, X), X)
    end.

good_enough(Guess, X) ->
    abs(square(Guess) - X) < ?TOLERANCE.

improve(Guess, X) ->
    average(Guess, X / Guess).

square(X) ->
    X * X.

average(X, Y) ->
    (X + Y) / 2.

test() ->
    true = abs(sqrt(4) - 2) < ?TOLERANCE,
    true = abs(sqrt(9) - 3) < ?TOLERANCE,
    test_ok.