-module(pi).
-compile(export_all).

-define(RANGE, 100000).

estimate_pi(N) ->
    random:seed(erlang:now()),
    math:sqrt(6 / monte_carlo(N, cesaro(?RANGE))).

cesaro(N) ->
    fun() ->
        gcd(random:uniform(N), random:uniform(N)) =:= 1
    end.

monte_carlo(N, Fun) ->
    monte_carlo(N, Fun, 0, N).

monte_carlo(0, _Fun, Cnt, Total) ->
    Cnt / Total;
monte_carlo(N, Fun, Cnt, Total) ->
    case Fun() of
        true ->
            monte_carlo(N - 1, Fun, Cnt + 1, Total);
        false ->
            monte_carlo(N - 1, Fun, Cnt, Total)
    end.

gcd(X, 0) -> X;
gcd(X, Y) -> gcd(Y, X rem Y).


estimate_pi_2(N) ->
    math:sqrt(6 / random_gcd_test(N, erlang:now())).

random_gcd_test(N, Ran) ->
    random_gcd_test(N, Ran, 0, N).

random_gcd_test(0, _Ran, Cnt, Total) ->
    Cnt / Total;
random_gcd_test(N, Ran, Cnt, Total) ->
    {N1, Ran1} = random:uniform_s(?RANGE, Ran),
    {N2, Ran2} = random:uniform_s(?RANGE, Ran1),
    case gcd(N1, N2) of
        1 ->
            random_gcd_test(N - 1, Ran2, Cnt + 1, Total);
        _ ->
            random_gcd_test(N - 1, Ran2, Cnt, Total)
    end.
