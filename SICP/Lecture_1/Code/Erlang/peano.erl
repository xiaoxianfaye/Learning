-module(peano).
-compile(export_all).

add_i(0, Y) -> Y;
add_i(X, Y) -> add_i('-1'(X), '+1'(Y)).

add_r(0, Y) -> Y;
add_r(X, Y) -> '+1'(add_r('-1'(X), Y)).

'-1'(X) -> X - 1.

'+1'(X) -> X + 1.

test() ->
    3 = add_i(1, 2),
    3 = add_r(1, 2),
    test_ok.