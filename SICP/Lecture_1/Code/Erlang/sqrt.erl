-module(sqrt).
-compile(export_all).

-define(TOLERANCE, 0.001).

sqrt(X) -> 
    my_try(1, X).

my_try(Guess, X) ->
    case good_enough(Guess, X) of
        true ->
            Guess;
        false ->
            my_try(improve(Guess, X), X)
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