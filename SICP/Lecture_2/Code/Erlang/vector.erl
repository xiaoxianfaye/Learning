-module(vector).
-compile(export_all).

make_vector(X, Y) -> [X|Y].
xcor(P) -> hd(P).
ycor(P) -> tl(P).

add_vector(X, Y) ->
    make_vector(xcor(X) + xcor(Y), ycor(X) + ycor(Y)).

scale_vector(M, X) ->
    make_vector(M * xcor(X), M * ycor(X)).

test() ->
    [4|6] = add_vector(make_vector(1, 2), make_vector(3, 4)),
    [2|4] = scale_vector(2, make_vector(1, 2)),

    test_ok.