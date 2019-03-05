-module(segment).
-compile(export_all).

make_vector(X, Y) -> [X|Y].
xcor(P) -> hd(P).
ycor(P) -> tl(P).

make_seg(P, Q) -> [P|Q].
seg_start(S) -> hd(S).
seg_end(S) -> tl(S).

midpoint(S) ->
    P = seg_start(S),
    Q = seg_end(S),
    make_vector(average(xcor(P), xcor(Q)),
                average(ycor(P), ycor(Q))).

slength(S) ->
    P = seg_start(S),
    Q = seg_end(S),
    Dx = xcor(P) - xcor(Q),
    Dy = ycor(P) - ycor(Q),
    fixed_points:sqrt(square(Dx) + square(Dy)).

average(X, Y) -> (X + Y) / 2.

square(X) -> X * X.

add_vector(X, Y) ->
    make_vector(xcor(X) + xcor(Y), ycor(X) + ycor(Y)).

scale_vector(M, X) ->
    make_vector(M * xcor(X), M * ycor(X)).

test() ->
    [2.0|1.0] = midpoint(make_seg(make_vector(0, 0), make_vector(4, 2))),
    5.0 = slength(make_seg(make_vector(0, 0), make_vector(4, 3))),

    [4|6] = add_vector(make_vector(1, 2), make_vector(3, 4)),
    [2|4] = scale_vector(2, make_vector(1, 2)),

    test_ok.
