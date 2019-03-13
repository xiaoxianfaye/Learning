-module(picture_rectangle).
-compile(export_all).

make_vector(X, Y) -> [X|Y].
xcor(P) -> hd(P).
ycor(P) -> tl(P).

add_vector(X, Y) ->
    make_vector(xcor(X) + xcor(Y), ycor(X) + ycor(Y)).

scale_vector(M, X) ->
    make_vector(M * xcor(X), M * ycor(X)).

make_seg(P, Q) -> [P|Q].
seg_start(S) -> hd(S).
seg_end(S) -> tl(S).

make_rect(Origin, Horiz, Vert) -> {Origin, Horiz, Vert}.
origin({Origin, _Horiz, _Vert}) -> Origin.
horiz({_Origin, Horiz, _Vert}) -> Horiz.
vert({_Origin, _Horiz, Vert}) -> Vert.

coord_map(Rect) ->
    fun(P) ->
        OriV = add_vector(
                origin(Rect),
                add_vector(
                    scale_vector(xcor(P), horiz(Rect)),
                    scale_vector(ycor(P), vert(Rect)))),
        make_vector(round(xcor(OriV)), round(ycor(OriV)))
    end.

% make_pict(Segs) ->
%     fun(Rect) ->
%         CM = coord_map(Rect),
%         lists:foreach(fun(Seg) -> 
%                         drawline(CM(seg_start(Seg)), CM(seg_end(Seg)))
%                       end,
%                       Segs)
%     end.

% make_pict(Segs) ->
%     fun(Rect) ->
%         fun(Draw) ->
%             CM = coord_map(Rect),
%             lists:foreach(fun(Seg) -> 
%                             Draw(CM(seg_start(Seg)), CM(seg_end(Seg)))
%                           end,
%                           Segs)
%         end
%     end.

% draw_pict() ->
%     Segs = [make_seg(make_vector(0, 0),      make_vector(0.5, 0)),
%             make_seg(make_vector(0, 0),      make_vector(0.25, 0.5)),
%             make_seg(make_vector(0.25, 0.5), make_vector(0.5, 0)),
%             make_seg(make_vector(0.5, 1),    make_vector(0.75, 0.5)),
%             make_seg(make_vector(0.5, 1),    make_vector(1, 1)),
%             make_seg(make_vector(0.75, 0.5), make_vector(1, 1)),
%             make_seg(make_vector(0.25, 0.5), make_vector(0.75, 0.5))],
%     P = make_pict(Segs)
%     Rect = make_rect(Origin, Horiz, Vert),
%     (P(Rect))(fun(X, Y) ->
%                 ...
%               end).

make_pict(Segs) ->
    fun(Rect) ->
        CM = coord_map(Rect),
        [make_seg(CM(seg_start(Seg)), CM(seg_end(Seg))) || Seg <- Segs]
    end.

draw_pict(Segs, PictFileName) ->
    Image = egd:create(601, 601),
    Color = egd:color({0, 0, 255}),
    lists:foreach(fun(Seg) ->
                    egd:line(
                        Image, 
                        vector_to_point(seg_start(Seg)), 
                        vector_to_point(seg_end(Seg)), Color)
                  end, Segs),
    egd:save(egd:render(Image, png), PictFileName),
    egd:destroy(Image).

vector_to_point(P) ->
    {xcor(P), ycor(P)}.

beside(Pict1, Pict2, Ratio) ->
    fun(Rect) ->
        CMSegs1 = Pict1(make_rect(
                            origin(Rect),
                            scale_vector(Ratio, horiz(Rect)),
                            vert(Rect))),
        CMSegs2 = Pict2(make_rect(
                            add_vector(origin(Rect), scale_vector(Ratio, horiz(Rect))),
                            scale_vector(1 - Ratio, horiz(Rect)),
                            vert(Rect))),
        CMSegs1 ++ CMSegs2
    end.

above(Pict1, Pict2, Ratio) ->
    fun(Rect) ->
        CMSegs1 = Pict1(make_rect(
                            origin(Rect),
                            horiz(Rect),
                            scale_vector(Ratio, vert(Rect)))),
        CMSegs2 = Pict2(make_rect(
                            add_vector(origin(Rect), scale_vector(Ratio, vert(Rect))),
                            horiz(Rect),
                            scale_vector(1 - Ratio, vert(Rect)))),
        CMSegs1 ++ CMSegs2
    end.

%% turn left 90 degrees
rotate_90(Pict) ->
    fun(Rect) ->
        Pict(make_rect(
                add_vector(origin(Rect), vert(Rect)),
                scale_vector(-1, vert(Rect)),
                horiz(Rect)))
    end.

flip_horiz(Pict) ->
    fun(Rect) ->
        Pict(make_rect(
                add_vector(origin(Rect), vert(Rect)),
                horiz(Rect),
                scale_vector(-1, vert(Rect))))
    end.

flip_vert(Pict) ->
    fun(Rect) ->
        Pict(make_rect(
                add_vector(origin(Rect), horiz(Rect)),
                scale_vector(-1, horiz(Rect)),
                vert(Rect)))
    end.

flip_pairs(Pict) ->
    Pict1 = above(flip_horiz(Pict), Pict, 0.5),
    beside(flip_vert(Pict1), Pict1, 0.5).

right_push(Pict, 0, _Ratio) -> Pict;
right_push(Pict, N, Ratio) ->
    beside(Pict, right_push(Pict, N - 1, Ratio), Ratio).

below_push(Pict, 0, _Ratio) -> Pict;
below_push(Pict, N, Ratio) ->
    above(Pict, below_push(Pict, N - 1, Ratio), Ratio).

corner_push(Pict, 0, _Ratio) -> Pict;
corner_push(Pict, N, Ratio) ->
    TopLeft = below_push(Pict, N - 1, Ratio),
    BottomRight = right_push(Pict, N - 1, Ratio),
    TopRight = corner_push(Pict, N - 1, Ratio),
    above(beside(Pict, above(BottomRight, BottomRight, 0.5), Ratio),
          beside(beside(TopLeft, TopLeft, 0.5), TopRight, Ratio),
          Ratio).

square_limit(Pict, N, R) ->
    flip_pairs(corner_push(Pict, N, R)).

push(Comb) ->
    fun(Pict, N, Ratio) ->
        (repeated(
            fun(P) ->
                Comb(Pict, P, Ratio)
            end, 
            N))(Pict)
    end.

repeated(_Fun, 0) ->
    fun(Pict) ->
        Pict
    end;
repeated(Fun, N) ->
    fun(Pict) ->
        Fun((repeated(Fun, N - 1))(Pict))
    end.

right_push_2(Pict, N, Ratio) ->
    (push(fun beside/3))(Pict, N, Ratio). 

below_push_2(Pict, N, Ratio) ->
    (push(fun above/3))(Pict, N, Ratio).

segs() ->
    [make_seg(make_vector(0, 0),      make_vector(0.5, 0)),
     make_seg(make_vector(0, 0),      make_vector(0.25, 0.5)),
     make_seg(make_vector(0.25, 0.5), make_vector(0.5, 0)),
     make_seg(make_vector(0.5, 1),    make_vector(0.75, 0.5)),
     make_seg(make_vector(0.5, 1),    make_vector(1, 1)),
     make_seg(make_vector(0.75, 0.5), make_vector(1, 1)),
     make_seg(make_vector(0.25, 0.5), make_vector(0.75, 0.5))].

test_vector() ->
    [4|6] = add_vector(make_vector(1, 2), make_vector(3, 4)),
    [2|4] = scale_vector(2, make_vector(1, 2)),
    test_vector_ok.

test_segment() ->
    Seg = make_seg(make_vector(1, 2), make_vector(3, 4)),
    [1|2] = seg_start(Seg),
    [3|4] = seg_end(Seg),
    test_segment_ok.

test_rect() ->
    Rect = make_rect(make_vector(1, 2), make_vector(3, 0), make_vector(0, 4)),
    [1|2] = origin(Rect),
    [3|0] = horiz(Rect),
    [0|4] = vert(Rect),
    test_rect_ok.

test_coord_map() ->
    Rect = make_rect(make_vector(1, 2), make_vector(3, 0), make_vector(0, 4)),
    CoordMap = coord_map(Rect),
    [4|6] = CoordMap(make_vector(1, 1)),

    Rect2 = make_rect(make_vector(2, 1), make_vector(4, 0), make_vector(0, 3)),
    CoordMap2 = coord_map(Rect2),
    [6|4] = CoordMap2(make_vector(1, 1)),

    test_coord_map_ok.

test_make_pict() ->
    Rect1 = make_rect(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    [[[0|0],300|0], [[0|0],150|300], [[150|300],300|0],
     [[300|600],450|300], [[300|600],600|600], [[450|300],600|600],
     [[150|300],450|300]] = (make_pict(segs()))(Rect1),

    Rect2 = make_rect(make_vector(0, 0), make_vector(500, 0), make_vector(0, 300)),
    [[[0|0],250|0], [[0|0],125|150], [[125|150],250|0],
     [[250|300],375|150], [[250|300],500|300], [[375|150],500|300],
     [[125|150],375|150]] = (make_pict(segs()))(Rect2),

    Rect3 = make_rect(make_vector(0, 0), make_vector(300, 0), make_vector(0, 500)),
    [[[0|0],150|0], [[0|0],75|250], [[75|250],150|0], 
     [[150|500],225|250], [[150|500],300|500], [[225|250],300|500],
     [[75|250],225|250]] = (make_pict(segs()))(Rect3),

    Rect4 = make_rect(make_vector(50, 50), make_vector(300, 0), make_vector(0, 500)),
    [[[50|50],200|50], [[50|50],125|300], [[125|300],200|50],
     [[200|550],275|300], [[200|550],350|550], [[275|300],350|550],
     [[125|300],275|300]] = (make_pict(segs()))(Rect4),

    test_make_pict_ok.

test_beside() ->
    Rect = make_rect(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    Pict = make_pict(segs()),
    [[[0|0],150|0], [[0|0],75|300], [[75|300],150|0],
     [[150|600],225|300], [[150|600],300|600], [[225|300],300|600],
     [[75|300],225|300], 
     [[300|0],450|0], [[300|0],375|300], [[375|300],450|0],
     [[450|600],525|300], [[450|600],600|600], [[525|300],600|600],
     [[375|300],525|300]] = (beside(Pict, Pict, 0.5))(Rect),

     test_beside_ok.

test_above() ->
    Rect = make_rect(make_vector(50, 50), make_vector(400, 0), make_vector(0, 400)),
    Pict = make_pict(segs()),
    [[[50|50],250|50], [[50|50],150|100], [[150|100],250|50],
     [[250|150],350|100], [[250|150],450|150], [[350|100],450|150],
     [[150|100],350|100],
     [[50|150],250|150], [[50|150],150|300], [[150|300],250|150],
     [[250|450],350|300], [[250|450],450|450], [[350|300],450|450],
     [[150|300],350|300]] = (above(Pict, Pict, 0.25))(Rect),

     test_above_ok.

test_rotate_90() ->
    Rect = make_rect(make_vector(50, 50), make_vector(400, 0), make_vector(0, 400)),
    Pict = make_pict(segs()),
    [[[50|450],50|250], [[50|450],250|350], [[250|350],50|250],
     [[450|250],250|150], [[450|250],450|50], [[250|150],450|50],
     [[250|350],250|150]] = (rotate_90(Pict))(Rect),

    test_rotate_90_ok.

test_flip_horiz() ->
    Rect = make_rect(make_vector(50, 50), make_vector(300, 0), make_vector(0, 300)),
    Pict = make_pict(segs()),
    [[[50|350],200|350], [[50|350],125|200], [[125|200],200|350],
     [[200|50],275|200], [[200|50],350|50], [[275|200],350|50],
     [[125|200],275|200]] = (flip_horiz(Pict))(Rect),

    test_flip_horiz_ok.

test_flip_vert() ->
    Rect = make_rect(make_vector(50, 50), make_vector(300, 0), make_vector(0, 300)),
    Pict = make_pict(segs()),
    [[[350|50],200|50], [[350|50],275|200], [[275|200],200|50],
     [[200|350],125|200], [[200|350],50|350], [[125|200],50|350],
     [[275|200],125|200]] = (flip_vert(Pict))(Rect),

    test_flip_vert_ok.

test_draw_pict() ->
    Segs = [make_seg(make_vector(0, 0), make_vector(300, 300)), 
            make_seg(make_vector(300, 300), make_vector(400, 300))],
    draw_pict(Segs, "test_draw_pict_0.png"),

    Rect = make_rect(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    CMSegs = (make_pict(segs()))(Rect),
    draw_pict(CMSegs, "test_draw_pict_1.png"),

    Rect2 = make_rect(make_vector(0, 0), make_vector(500, 0), make_vector(0, 300)),
    CMSegs2 = (make_pict(segs()))(Rect2),
    draw_pict(CMSegs2, "test_draw_pict_2.png"),

    Rect3 = make_rect(make_vector(0, 0), make_vector(300, 0), make_vector(0, 500)),
    CMSegs3 = (make_pict(segs()))(Rect3),
    draw_pict(CMSegs3, "test_draw_pict_3.png"),

    Rect4 = make_rect(make_vector(50, 50), make_vector(300, 0), make_vector(0, 500)),
    CMSegs4 = (make_pict(segs()))(Rect4),
    draw_pict(CMSegs4, "test_draw_pict_4.png"),


    Pict = make_pict(segs()),

    Rect5 = make_rect(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    CMSegs5 = (beside(Pict, Pict, 0.5))(Rect5),
    draw_pict(CMSegs5, "test_draw_pict_5.png"),

    Rect6 = make_rect(make_vector(50, 50), make_vector(400, 0), make_vector(0, 400)),
    CMSegs6 = (above(Pict, Pict, 0.25))(Rect6),
    draw_pict(CMSegs6, "test_draw_pict_6.png"),

    Rect7 = make_rect(make_vector(50, 50), make_vector(400, 0), make_vector(0, 400)),
    CMSegs7 = (rotate_90(Pict))(Rect7),
    draw_pict(CMSegs7, "test_draw_pict_7.png"),

    Rect8 = make_rect(make_vector(50, 50), make_vector(300, 0), make_vector(0, 300)),
    CMSegs8 = (flip_horiz(Pict))(Rect8),
    draw_pict(CMSegs8, "test_draw_pict_8.png"),

    Rect9 = make_rect(make_vector(50, 50), make_vector(300, 0), make_vector(0, 300)),
    CMSegs9 = (flip_vert(Pict))(Rect9),
    draw_pict(CMSegs9, "test_draw_pict_9.png"),

    Rect10 = make_rect(make_vector(50, 50), make_vector(300, 0), make_vector(0, 300)),
    CMSegs10 = (flip_pairs(Pict))(Rect10),
    draw_pict(CMSegs10, "test_draw_pict_10.png"),

    Rect11 = make_rect(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    CMSegs11 = (right_push(Pict, 4, 0.5))(Rect11),
    draw_pict(CMSegs11, "test_draw_pict_11.png"),

    Rect12 = make_rect(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    CMSegs12 = (below_push(Pict, 4, 0.5))(Rect12),
    draw_pict(CMSegs12, "test_draw_pict_12.png"),

    Rect13 = make_rect(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    CMSegs13 = (corner_push(Pict, 4, 0.5))(Rect13),
    draw_pict(CMSegs13, "test_draw_pict_13.png"),

    Rect14 = make_rect(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    CMSegs14 = (square_limit(flip_pairs(Pict), 4, 0.5))(Rect14),
    draw_pict(CMSegs14, "test_draw_pict_14.png"),

    Rect15 = make_rect(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    CMSegs15 = (right_push_2(Pict, 4, 0.5))(Rect15),
    draw_pict(CMSegs15, "test_draw_pict_15.png"),

    Rect16 = make_rect(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    CMSegs16 = (below_push_2(Pict, 4, 0.5))(Rect16),
    draw_pict(CMSegs16, "test_draw_pict_16.png"),

    test_draw_pict_ok.

test() ->
    test_vector(),
    test_segment(),
    test_rect(),
    test_coord_map(),
    test_make_pict(),
    test_beside(),
    test_above(),
    test_rotate_90(),
    test_flip_horiz(),
    test_flip_vert(),
    test_draw_pict(),
    test_ok.