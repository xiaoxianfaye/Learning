-module(stream_cache).
-compile(export_all).

-type stream() :: {term(), fun(() -> stream())}.

%% Stream
-spec cons_stream(term(), fun(() -> stream())) -> stream().
cons_stream(H, T) ->
    Ref = make_ref(),
    io:format("make_ref ~p~n", [Ref]),
    F = fun() ->
            io:format("Ref ~p~n", [Ref]),
            case get(Ref) of
                undefined ->
                    Res = T(),
                    put(Ref, Res),
                    io:format("put {~p, ~p}~n", [Ref, Res]),
                    Res;
                R ->
                    io:format("get {~p, ~p}~n", [Ref, R]),
                    R
            end
        end,
    [H, F].

head([H, _]) -> H.
tail([_, T]) -> T().

the_empty_stream() -> [].

is_empty_stream([]) -> true;
is_empty_stream(_) -> false.

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

%% Infinite Stream
integers_from(N) ->
    cons_stream(N, fun() -> integers_from(N + 1) end). 

