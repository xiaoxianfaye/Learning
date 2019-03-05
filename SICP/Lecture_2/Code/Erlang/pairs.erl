-module(pairs).
-compile(export_all).

my_cons(X, Y) ->
    fun(0) -> X;
       (1) -> Y
    end.

my_hd(P) ->
    P(0).

my_tl(P) ->
    P(1).

test() ->
    P = my_cons(1, 2),
    1 = my_hd(P),
    2 = my_tl(P),
    test_ok.