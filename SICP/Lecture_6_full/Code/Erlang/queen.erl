-module(queen).
-compile(export_all).

-import(stream, [cons_stream/2, the_empty_stream/0, map_stream/2, 
                 filter_stream/2, enumerate_interval/2, flatmap/2]).

%% Backtracking, stop after the first solution gotten
bt_queen(N) ->
    bt_queen(N, 1, 1, the_empty_board()).

bt_queen(TR, CurRow, _CurCol, CurPos) when CurRow > TR ->
    CurPos;
bt_queen(TR, _CurRow, CurCol, CurPos) when CurCol > TR ->
    case split_position(CurPos) of
        {{LastRow, LastCol}, RestPos} ->
            bt_queen(TR, LastRow, LastCol + 1, RestPos);
        none ->
            no_solutions
    end;
bt_queen(TR, CurRow, CurCol, CurPos) ->
    NewPoses = adjoin_position(CurRow, CurCol, CurPos),
    case is_safe(NewPoses) of
        true ->
            bt_queen(TR, CurRow + 1, 1, NewPoses);
        false ->
            bt_queen(TR, CurRow, CurCol + 1, CurPos)
    end.

split_position([]) -> none;
split_position(Pos) -> {hd(Pos), tl(Pos)}.

the_empty_board() -> [].

adjoin_position(Row, Col, RestPos) -> [{Row, Col} | RestPos].

is_safe([{Row, Col}|RestPos]) ->
    case lists:keyfind(Col, 2, RestPos) of
        false ->
            Diff = [{abs(Row - Row1), abs(Col - Col1)} || {Row1, Col1} <- RestPos],
            lists:all(fun({Dif, Dif}) -> false; 
                         (_) -> true
                      end, Diff);
        _ -> 
            false
    end.

%% Backtracking, all solutions
bt_queen_2(N) ->
    bt_queen_2(N, 1, 1, the_empty_board(), []).

bt_queen_2(TR, CurRow, _CurCol, CurPos, Solutions) when CurRow > TR ->
    [{LastRow, LastCol} | RestPos] = CurPos,
    bt_queen_2(TR, LastRow, LastCol + 1, RestPos, [CurPos|Solutions]);
bt_queen_2(TR, _CurRow, CurCol, CurPos, Solutions) when CurCol > TR ->
    case split_position(CurPos) of
        {{LastRow, LastCol}, RestPos} ->
            bt_queen_2(TR, LastRow, LastCol + 1, RestPos, Solutions);
        none ->
            if
                Solutions =:= [] -> no_solutions;
                true -> Solutions
            end
    end;
bt_queen_2(TR, CurRow, CurCol, CurPos, Solutions) ->
    NewPoses = adjoin_position(CurRow, CurCol, CurPos),
    case is_safe(NewPoses) of
        true ->
            bt_queen_2(TR, CurRow + 1, 1, NewPoses, Solutions);
        false ->
            bt_queen_2(TR, CurRow, CurCol + 1, CurPos, Solutions)
    end.

%% Recursive, all solutions
re_queen(N) -> fill_rows(N, N).

fill_rows(_Size, 0) ->
    cons_stream(the_empty_board(), the_empty_stream());
fill_rows(Size, Row) ->
    filter_stream(
        fun is_safe/1,
        flatmap(
            fun(RestPos) ->
                map_stream(
                    fun(Col) ->
                        % io:format("re_queen Row ~p Col ~p RestPos ~p~n", [Row, Col, RestPos]),
                        adjoin_position(Row, Col, RestPos)
                    end, 
                    enumerate_interval(1, Size))
            end, 
            fill_rows(Size, Row - 1))).


re_queen_2(N) -> fill_rows_2(N, N).

fill_rows_2(_Size, 0) ->
    cons_stream(the_empty_board(), the_empty_stream());
fill_rows_2(Size, Row) ->
    filter_stream(
        fun is_safe/1,
        flatmap(
            fun(Col) ->
                map_stream(
                    fun(RestPos) -> 
                        % io:format("re_queen_2 Row ~p Col ~p RestPos ~p~n", [Row, Col, RestPos]),
                        adjoin_position(Row, Col, RestPos)
                    end, 
                    fill_rows_2(Size, Row - 1))
            end, 
            enumerate_interval(1, Size))).

test_bt_queen() ->
    [{8, 4}, {7, 2}, {6, 7}, {5, 3}, {4, 6}, {3, 8}, {2, 5}, {1, 1}] =  bt_queen(8),

    no_solutions = bt_queen(3),

    [{4, 3}, {3, 1}, {2, 4}, {1, 2}] = bt_queen(4),

    test_bt_queen_ok.

test_bt_queen_2() ->
    92 = length(bt_queen_2(8)),
    2 = length(bt_queen_2(4)),
    no_solutions = bt_queen_2(3),

    test_bt_queen_2_ok.

test_re_queen() ->
    92 = length(re_queen(8)),
    2 = length(re_queen(4)),
    [] = re_queen(3),

    test_re_queen_ok.

test_re_queen_2() ->
    92 = length(re_queen_2(8)),
    2 = length(re_queen_2(4)),
    [] = re_queen_2(3),

    test_re_queen_2_ok.

test() ->
    test_bt_queen(),
    test_bt_queen_2(),
    test_re_queen(),
    test_re_queen_2(),
    test_ok.

profile(Prompt, Func, N) ->
    {TimeCost, _} = timer:tc(queen, Func, [N]),
    io:format("~p : ~p~n", [Prompt, TimeCost]).

profile() ->
    profile("re_queen RestPos -> Col", re_queen, 3),
    profile("re_queen_2 Col -> RestPos", re_queen_2, 3).


