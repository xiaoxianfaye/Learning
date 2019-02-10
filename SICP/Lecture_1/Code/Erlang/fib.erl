-module(fib).
-compile(export_all).

fib_r(N) when N < 2 -> N;
fib_r(N) -> fib_r(N - 1) + fib_r(N - 2).

fib_i(N) -> fib_i(N, 0, 1).

fib_i(0, A1, _) -> A1;
fib_i(1, _, A2) -> A2;
fib_i(N, A1, A2) -> fib_i(N - 1, A2, A1 + A2).

test() ->
    [0, 1, 3, 5, 8] = [fib_r(0), fib_r(1), fib_r(4), fib_r(5), fib_r(6)],
    [0, 1, 3, 5, 8] = [fib_i(0), fib_i(1), fib_i(4), fib_i(5), fib_i(6)],
    test_ok.