-module(stream_cache).
-compile(export_all).

-type stream() :: {term(), fun(() -> stream())}.

%% Stream
-spec cons_stream(term(), fun(() -> stream())) -> stream().
cons_stream(H, T) ->
    Ref = make_ref(),
    % io:format("make_ref ~p~n", [Ref]),
    F = fun() ->
            % io:format("Ref ~p~n", [Ref]),
            case get(Ref) of
                undefined ->
                    Res = T(),
                    put(Ref, Res),
                    % io:format("put {~p, ~p}~n", [Ref, Res]),
                    Res;
                R ->
                    % io:format("get {~p, ~p}~n", [Ref, R]),
                    put(cnt, get(cnt) + 1),
                    R
            end
        end,
    [H, F].

head([H, _]) -> H.
tail([_, T]) -> T().

the_empty_stream() -> [].

is_empty_stream([]) -> true;
is_empty_stream(_) -> false.

add_stream(S1, S2) ->
    case {is_empty_stream(S1), is_empty_stream(S2)} of
        {true, true} ->
            the_empty_stream();
        {true, false} ->
            S2;
        {false, true} ->
            S1;
        {false, false} ->
            cons_stream(
                head(S1) + head(S2),
                fun() ->
                    add_stream(tail(S1), tail(S2))
                end)
    end.

%% Terminals
nth_stream(Idx, S) ->
    case is_empty_stream(S) of
        true ->
            throw(no_this_idx);
        false ->
            if
                Idx =:= 1 ->
                    head(S);
                true ->
                    nth_stream(Idx - 1, tail(S))
            end
    end.

print_stream(_S, 0) -> ok;
print_stream(S, N) ->
    case is_empty_stream(S) of
        true ->
            io:format("Done~n");
        false ->
            io:format("~p ", [head(S)]),
            print_stream(tail(S), N - 1)
    end.    

%% Infinite Stream
integers_from(N) ->
    cons_stream(N, fun() -> integers_from(N + 1) end). 

% Defining streams implicitly
fibs() ->
    cons_stream(0,
                fun() ->
                    cons_stream(1,
                                fun() ->
                                    add_stream(fibs(), tail(fibs()))
                                end)
                end).

%% Pseudo simulation using process dictionary
naming_cons_stream(N, H, T) ->
    put(N, cons_stream(H, T)),
    get(N).

fibs_naming() ->
    naming_cons_stream(
        fibs,
        0,
        fun() ->
            cons_stream(
                1,
                fun() ->
                    Fibs = get(fibs),
                    add_stream(Fibs, tail(Fibs))
                end)
        end
    ).
