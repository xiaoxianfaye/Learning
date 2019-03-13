-module(picture_frame).
-compile(export_all).

%% Language of primitive picture

% Vector
make_vector(X, Y) -> [X|Y].
xcor(P) -> hd(P).
ycor(P) -> tl(P).

add_vector(P1, P2) ->
    make_vector(xcor(P1) + xcor(P2), ycor(P1) + ycor(P2)).

sub_vector(P1, P2) ->
    make_vector(xcor(P1) - xcor(P2), ycor(P1) - ycor(P2)).
    
scale_vector(Scale, P) ->
    make_vector(Scale * xcor(P), Scale * ycor(P)).

% Segment
make_seg(StartP, EndP) -> [StartP|EndP].
seg_start(Seg) -> hd(Seg).
seg_end(Seg) -> tl(Seg).

% Frame
make_frame(Origin, Edge1, Edge2) -> {Origin, Edge1, Edge2}.
origin({Origin, _Edge1, _Edge2}) -> Origin.
edge1({_Origin, Edge1, _Edge2}) -> Edge1.
edge2({_Origin, _Edge1, Edge2}) -> Edge2.

% Coordination mapping from the unit square to the frame
coord_map(Frame) ->
    fun(P) ->
        OriV = add_vector(
                origin(Frame),
                add_vector(
                    scale_vector(xcor(P), edge1(Frame)),
                    scale_vector(ycor(P), edge2(Frame)))),
        make_vector(round(xcor(OriV)), round(ycor(OriV)))
    end.

% Painter
make_painter(Segs) ->
    fun(Frame) ->
        CM = coord_map(Frame),
        [make_seg(CM(seg_start(Seg)), CM(seg_end(Seg))) || Seg <- Segs]
    end.


% Draw
draw(Segs, FileName) ->
    Image = egd:create(601, 601),
    Color = egd:color({0, 0, 255}),
    lists:foreach(fun(Seg) ->
                    egd:line(
                        Image, 
                        vector_to_point(seg_start(Seg)), 
                        vector_to_point(seg_end(Seg)), Color)
                  end, Segs),
    egd:save(egd:render(Image, png), FileName),
    egd:destroy(Image).

vector_to_point(P) ->
    {xcor(P), ycor(P)}.

%% Language of Geometric positions

% Transform unit square painter to frame painter
transform_painter(Painter, Origin, Corner1, Corner2) ->
    fun(Frame) ->
        CM = coord_map(Frame),
        NewOri = CM(Origin),
        Edge1 = sub_vector(CM(Corner1), NewOri),
        Edge2 = sub_vector(CM(Corner2), NewOri),
        Painter(make_frame(NewOri, Edge1, Edge2))
    end.

beside(P1, P2, Ratio) ->
    fun(Frame) ->
        TransP1 = transform_painter(P1, make_vector(0, 0), make_vector(Ratio, 0), make_vector(0, 1)),
        TransP2 = transform_painter(P2, make_vector(Ratio, 0), make_vector(1, 0), make_vector(Ratio, 1)),
        TransP1(Frame) ++ TransP2(Frame)
    end.

% beside(P1, P2, Ratio) ->
%     fun(Frame) ->
%         CMSegs1 = P1(make_frame(
%                         origin(Frame),
%                         scale_vector(Ratio, edge1(Frame)),
%                         edge2(Frame))),
%         CMSegs2 = P2(make_frame(
%                         add_vector(origin(Frame), scale_vector(Ratio, edge1(Frame))),
%                         scale_vector(1 - Ratio, edge1(Frame)),
%                         edge2(Frame))),
%         CMSegs1 ++ CMSegs2
%     end.

above(P1, P2, Ratio) ->
    fun(Frame) ->
        TransP1 = transform_painter(P1, make_vector(0, 0), make_vector(1, 0), make_vector(0, Ratio)),
        TransP2 = transform_painter(P2, make_vector(0, Ratio), make_vector(1, Ratio), make_vector(0, 1)),
        TransP1(Frame) ++ TransP2(Frame)
    end.

empty() ->
    fun(_Frame) ->
        []
    end.

% anti-clockwise
rotate_90(P) ->
    transform_painter(P, make_vector(0, 1), make_vector(0, 0), make_vector(1, 1)).

flip_horiz(P) ->
    transform_painter(P, make_vector(0, 1), make_vector(1, 1), make_vector(0, 0)).    

flip_vert(P) ->
    transform_painter(P, make_vector(1, 0), make_vector(0, 0), make_vector(1, 1)).

shrink_to_bottom(P) ->
    transform_painter(P, make_vector(0, 0.5), make_vector(1, 0.5), make_vector(0, 1)).

%% Language of schemes of combination

flip_pairs(P) ->
    NewP = above(flip_horiz(P), P, 0.5),
    beside(flip_vert(NewP), NewP, 0.5).

push(Comb) ->
    fun(P, N, Ratio) ->
        (repeated(
            fun(P1) ->
                Comb(P, P1, Ratio)
            end, N))(P)
    end.

repeated(_F, 0) ->
    fun(P) ->
        P
    end;
repeated(F, N) ->
    fun(P) ->
        F((repeated(F, N - 1))(P))
    end.

right_push(P, N, Ratio) ->
    (push(fun beside/3))(P, N, Ratio).

below_push(P, N, Ratio) ->
    (push(fun above/3))(P, N, Ratio).

corner_push(P, 0, _Ratio) ->
    P;
corner_push(P, N, Ratio) ->
    TopLeft = below_push(P, N - 1, Ratio),
    BottomRight = right_push(P, N - 1, Ratio),
    TopRight = corner_push(P, N - 1, Ratio),
    above(beside(P, above(BottomRight, BottomRight, 0.5), Ratio),
          beside(beside(TopLeft, TopLeft, 0.5), TopRight, Ratio),
          Ratio).

square_limit(P, N, Ratio) ->
    flip_pairs(corner_push(P, N, Ratio)).

big_and_small(P) ->
    beside(P, above(empty(), P, 0.5), 0.5).

big_and_small_2(P) ->
    beside(P, shrink_to_bottom(P), 0.5).

squash_inwards(P) ->
    transform_painter(P, make_vector(0, 0), make_vector(0.35, 0.65), make_vector(0.65, 0.35)).

%% Segments which are used to make painter
segs() ->
    [make_seg(make_vector(0, 0),      make_vector(0.5, 0)),
     make_seg(make_vector(0, 0),      make_vector(0.25, 0.5)),
     make_seg(make_vector(0.25, 0.5), make_vector(0.5, 0)),
     make_seg(make_vector(0.5, 1),    make_vector(0.75, 0.5)),
     make_seg(make_vector(0.5, 1),    make_vector(1, 1)),
     make_seg(make_vector(0.75, 0.5), make_vector(1, 1)),
     make_seg(make_vector(0.25, 0.5), make_vector(0.75, 0.5))].

%% Test
test_vector() ->
    [4|6] = add_vector(make_vector(1, 2), make_vector(3, 4)),
    [2|3] = sub_vector(make_vector(3, 5), make_vector(1, 2)),
    [2|4] = scale_vector(2, make_vector(1, 2)),
    test_vector_ok.

test_seg() ->
    Seg = make_seg(make_vector(1, 2), make_vector(3, 4)),
    [1|2] = seg_start(Seg),
    [3|4] = seg_end(Seg),
    test_seg_ok.

test_frame() ->
    Frame = make_frame(make_vector(1, 2), make_vector(3, 0), make_vector(0, 4)),
    [1|2] = origin(Frame),
    [3|0] = edge1(Frame),
    [0|4] = edge2(Frame),
    test_frame_ok.

test_coord_map() ->
    Frame1 = make_frame(make_vector(1, 2), make_vector(3, 0), make_vector(0, 4)),
    CoordMap1 = coord_map(Frame1),
    [4|6] = CoordMap1(make_vector(1, 1)),

    Frame2 = make_frame(make_vector(1, 2), make_vector(3, 1), make_vector(2, 4)),
    CoordMap2 = coord_map(Frame2),
    [6|7] = CoordMap2(make_vector(1, 1)),

    Frame3 = make_frame(make_vector(2, 1), make_vector(4, 1), make_vector(2, 3)),
    CoordMap3 = coord_map(Frame3),
    [8|5] = CoordMap3(make_vector(1, 1)),

    test_coord_map_ok.

test_make_painter() ->
    Frame1 = make_frame(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    [[[0|0],300|0], [[0|0],150|300], [[150|300],300|0],
     [[300|600],450|300], [[300|600],600|600], [[450|300],600|600],
     [[150|300],450|300]] = (make_painter(segs()))(Frame1),

    Frame2 = make_frame(make_vector(50, 50), make_vector(350, 100), make_vector(200, 450)),
    [[[50|50],225|100], [[50|50],238|300], [[238|300],225|100],
     [[425|550],413|350], [[425|550],600|600], [[413|350],600|600],
     [[238|300],413|350]] = (make_painter(segs()))(Frame2),

    test_make_painter_ok.

test_transform_painter() ->
    Painter = make_painter(segs()),

    Frame1 = make_frame(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    TransPainter1 = transform_painter(Painter, make_vector(0, 0), make_vector(1, 0), make_vector(0, 1)),
    [[[0|0],300|0], [[0|0],150|300], [[150|300],300|0],
     [[300|600],450|300], [[300|600],600|600], [[450|300],600|600],
     [[150|300],450|300]] = TransPainter1(Frame1),

    Frame2 = make_frame(make_vector(50, 50), make_vector(350, 100), make_vector(200, 450)),
    TransPainter2 = transform_painter(Painter, make_vector(0, 0), make_vector(1, 0), make_vector(0, 1)),
    [[[50|50],225|100], [[50|50],238|300], [[238|300],225|100],
     [[425|550],413|350], [[425|550],600|600], [[413|350],600|600],
     [[238|300],413|350]] = TransPainter2(Frame2),

    Frame3 = make_frame(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    TransPainter3 = transform_painter(Painter, make_vector(0, 0), make_vector(0.5, 0), make_vector(0, 1)),
    [[[0|0],150|0], [[0|0],75|300], [[75|300],150|0],
     [[150|600],225|300], [[150|600],300|600], [[225|300],300|600],
     [[75|300],225|300]] = TransPainter3(Frame3),

    Frame4 = make_frame(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    TransPainter4 = transform_painter(Painter, make_vector(0.5, 0), make_vector(1, 0), make_vector(0.5, 1)),
    [[[300|0],450|0], [[300|0],375|300], [[375|300],450|0],
     [[450|600],525|300], [[450|600],600|600], [[525|300],600|600],
     [[375|300],525|300]] = TransPainter4(Frame4),

    test_transform_painter_ok.

test_beside() ->
    Frame1 = make_frame(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    Painter1 = make_painter(segs()),
    [[[0|0],150|0], [[0|0],75|300], [[75|300],150|0],
     [[150|600],225|300], [[150|600],300|600], [[225|300],300|600],
     [[75|300],225|300], 
     [[300|0],450|0], [[300|0],375|300], [[375|300],450|0],
     [[450|600],525|300], [[450|600],600|600], [[525|300],600|600],
     [[375|300],525|300]] = (beside(Painter1, Painter1, 0.5))(Frame1),

    Frame2 = make_frame(make_vector(50, 50), make_vector(350, 100), make_vector(200, 450)),
    Painter2 = (make_painter(segs())),
    [[[50|50],138|75], [[50|50],194|288], [[194|288],138|75],
     [[338|525],281|313], [[338|525],425|550], [[281|313],425|550],
     [[194|288],281|313],
     [[225|100],313|125], [[225|100],369|338], [[369|338],313|125], 
     [[513|575],456|363], [[513|575],600|600], [[456|363],600|600],
     [[369|338],456|363]] = (beside(Painter2, Painter2, 0.5))(Frame2),

    test_beside_ok.

test_above() ->
    Frame1 = make_frame(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    Painter1 = make_painter(segs()),
    [[[0|0],300|0], [[0|0],150|75], [[150|75],300|0],
     [[300|150],450|75], [[300|150],600|150], [[450|75],600|150],
     [[150|75],450|75],
     [[0|150],300|150], [[0|150],150|375], [[150|375],300|150],
     [[300|600],450|375], [[300|600],600|600], [[450|375],600|600],
     [[150|375],450|375]] = (above(Painter1, Painter1, 0.25))(Frame1),

    Frame2 = make_frame(make_vector(50, 50), make_vector(350, 100), make_vector(200, 450)),
    Painter2 = (make_painter(segs())),
    [[[50|50],225|100], [[50|50],163|132], [[163|132],225|100],
     [[275|213],338|182], [[275|213],450|263], [[338|182],450|263],
     [[163|132],338|182],
     [[100|163],275|213], [[100|163],263|357], [[263|357],275|213],
     [[425|550],438|407], [[425|550],600|600], [[438|407],600|600],
     [[263|357],438|407]] = (above(Painter2, Painter2, 0.25))(Frame2),

    test_above_ok.

test_rotate_90() ->
    Frame1 = make_frame(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    Painter1 = make_painter(segs()),
    [[[0|600],0|300], [[0|600],300|450], [[300|450],0|300],
     [[600|300],300|150], [[600|300],600|0], [[300|150],600|0],
     [[300|450],300|150]] = (rotate_90(Painter1))(Frame1),

    Frame2 = make_frame(make_vector(50, 50), make_vector(350, 100), make_vector(200, 450)),
    Painter2 = (make_painter(segs())),
    [[[250|500],150|275], [[250|500],375|438], [[375|438],150|275],
     [[500|375],275|213], [[500|375],400|150], [[275|213],400|150],
     [[375|438],275|213]] = (rotate_90(Painter2))(Frame2),

    test_rotate_90_ok.

test_flip_horiz() ->
    Frame1 = make_frame(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    Painter1 = make_painter(segs()),
    [[[0|600],300|600], [[0|600],150|300], [[150|300],300|600],
     [[300|0],450|300], [[300|0],600|0], [[450|300],600|0],
     [[150|300],450|300]] = (flip_horiz(Painter1))(Frame1),

    Frame2 = make_frame(make_vector(50, 50), make_vector(350, 100), make_vector(200, 450)),
    Painter2 = (make_painter(segs())),
    [[[250|500],425|550], [[250|500],238|300], [[238|300],425|550],
     [[225|100],413|350], [[225|100],400|150], [[413|350],400|150],
     [[238|300],413|350]] = (flip_horiz(Painter2))(Frame2),

    test_flip_horiz_ok.

test_flip_vert() ->
    Frame1 = make_frame(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    Painter1 = make_painter(segs()),
    [[[600|0],300|0], [[600|0],450|300], [[450|300],300|0],
     [[300|600],150|300], [[300|600],0|600], [[150|300],0|600],
     [[450|300],150|300]] = (flip_vert(Painter1))(Frame1),

    Frame2 = make_frame(make_vector(50, 50), make_vector(350, 100), make_vector(200, 450)),
    Painter2 = (make_painter(segs())),
    [[[400|150],225|100], [[400|150],413|350], [[413|350],225|100],
     [[425|550],238|300], [[425|550],250|500], [[238|300],250|500],
     [[413|350],238|300]] = (flip_vert(Painter2))(Frame2),

    test_flip_vert_ok.

test_shrink_to_bottom() ->
    Frame1 = make_frame(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    Painter1 = make_painter(segs()),
    [[[0|300],300|300], [[0|300],150|450], [[150|450],300|300],
     [[300|600],450|450], [[300|600],600|600], [[450|450],600|600],
     [[150|450],450|450]] = (shrink_to_bottom(Painter1))(Frame1),

    Frame2 = make_frame(make_vector(50, 50), make_vector(350, 100), make_vector(200, 450)),
    Painter2 = (make_painter(segs())),
    [[[150|275],325|325], [[150|275],288|413], [[288|413],325|325],
     [[425|550],463|463], [[425|550],600|600], [[463|463],600|600],
     [[288|413],463|463]] = (shrink_to_bottom(Painter2))(Frame2),

    test_shrink_to_bottom_ok.

test_draw() ->
    Painter = make_painter(segs()),
    Rectangle = make_frame(make_vector(0, 0), make_vector(600, 0), make_vector(0, 600)),
    Diamond = make_frame(make_vector(50, 50), make_vector(350, 100), make_vector(200, 450)),

    draw(Painter(Rectangle), "test_draw_painter_1.png"),
    draw(Painter(Diamond), "test_draw_painter_2.png"),

    BesidePainter = beside(Painter, Painter, 0.5),
    draw(BesidePainter(Rectangle), "test_draw_painter_3.png"),
    draw(BesidePainter(Diamond), "test_draw_painter_4.png"),

    AbovePainter = above(Painter, Painter, 0.25),
    draw(AbovePainter(Rectangle), "test_draw_painter_5.png"),
    draw(AbovePainter(Diamond), "test_draw_painter_6.png"),

    Rotate90Painter = rotate_90(Painter),
    draw(Rotate90Painter(Rectangle), "test_draw_painter_7.png"),
    draw(Rotate90Painter(Diamond), "test_draw_painter_8.png"),

    FlipHorizPainter = flip_horiz(Painter),
    draw(FlipHorizPainter(Rectangle), "test_draw_painter_9.png"),
    draw(FlipHorizPainter(Diamond), "test_draw_painter_10.png"),

    FlipVertPainter = flip_vert(Painter),
    draw(FlipVertPainter(Rectangle), "test_draw_painter_11.png"),
    draw(FlipVertPainter(Diamond), "test_draw_painter_12.png"),

    FlipPairsPainter = flip_pairs(Painter),
    draw(FlipPairsPainter(Rectangle), "test_draw_painter_13.png"),
    draw(FlipPairsPainter(Diamond), "test_draw_painter_14.png"),

    RightPushPainter = right_push(Painter, 4, 0.5),
    draw(RightPushPainter(Rectangle), "test_draw_painter_15.png"),
    draw(RightPushPainter(Diamond), "test_draw_painter_16.png"),

    BelowPushPainter = below_push(Painter, 4, 0.5),
    draw(BelowPushPainter(Rectangle), "test_draw_painter_17.png"),
    draw(BelowPushPainter(Diamond), "test_draw_painter_18.png"),

    CornerPushPainter = corner_push(Painter, 4, 0.5),
    draw(CornerPushPainter(Rectangle), "test_draw_painter_19.png"),
    draw(CornerPushPainter(Diamond), "test_draw_painter_20.png"),

    SquareLimitPainter = square_limit(flip_pairs(Painter), 4, 0.5),
    draw(SquareLimitPainter(Rectangle), "test_draw_painter_21.png"),
    draw(SquareLimitPainter(Diamond), "test_draw_painter_22.png"),

    ShrinkToBottomPainter = shrink_to_bottom(Painter),
    draw(ShrinkToBottomPainter(Rectangle), "test_draw_painter_23.png"),
    draw(ShrinkToBottomPainter(Diamond), "test_draw_painter_24.png"),

    BigAndSmallPainter = big_and_small(Painter),
    draw(BigAndSmallPainter(Rectangle), "test_draw_painter_25.png"),
    draw(BigAndSmallPainter(Diamond), "test_draw_painter_26.png"),

    BigAndSmall2Painter = big_and_small_2(Painter),
    draw(BigAndSmall2Painter(Rectangle), "test_draw_painter_27.png"),
    draw(BigAndSmall2Painter(Diamond), "test_draw_painter_28.png"),

    SquashInwardsPainter = squash_inwards(Painter),
    draw(SquashInwardsPainter(Rectangle), "test_draw_painter_29.png"),
    draw(SquashInwardsPainter(Diamond), "test_draw_painter_30.png"),

    SquashInSquareLimitPainter = square_limit(flip_pairs(squash_inwards(Painter)), 4, 0.5),
    draw(SquashInSquareLimitPainter(Rectangle), "test_draw_painter_31.png"),
    draw(SquashInSquareLimitPainter(Diamond), "test_draw_painter_32.png"),

    SquashInSquareLimitPainter2 = square_limit(flip_pairs(squash_inwards(Painter)), 6, 0.5),
    draw(SquashInSquareLimitPainter2(Rectangle), "test_draw_painter_33.png"),
    draw(SquashInSquareLimitPainter2(Diamond), "test_draw_painter_34.png"),

    test_draw_ok.

test() ->
    test_vector(),
    test_seg(),
    test_frame(),
    test_coord_map(),
    test_make_painter(),
    test_transform_painter(),
    test_beside(),
    test_above(),
    test_rotate_90(),
    test_flip_horiz(),
    test_flip_vert(),
    test_shrink_to_bottom(),
    test_draw(),
    test_ok.
