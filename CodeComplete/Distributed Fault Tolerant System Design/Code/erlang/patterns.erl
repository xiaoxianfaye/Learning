-module(patterns).
-compile(export_all).

promote({ceo, male}) -> "man CEO now";
promote({ceo, female}) -> "woman CEO now";
promote({ceo, _}) -> "No CEO for you!".

demote(Arg) when is_tuple(Arg) ->
    case Arg of
        {ceo, male} -> "Fire the CEO";
        {ceo, female} -> "Fire the CEO";
        {ceo, _} -> "Cannot fire non existing CEO";
        _ -> "unknown pattern"
    end.

postman() ->
    receive
        {send, {Fr, To, _Content} = Pkg} ->
            io:format("~s sending something to ~s ~n", [Fr, To]),
            self() ! {recv, Pkg},
            postman();
        {recv, {To, Fr, Content} = _Pkg} ->
            io:format("~s got a ~s from ~s ~n", [Fr, Content, To]),
            postman();
        stop ->
            io:format("Shutting down postman ...~n")
    end.

is_even(Number) ->
    Type = try Number rem 2 of
        0 when is_number(Number) -> true;
        1 when is_number(Number) -> false
    catch
        _ErrType:_Err -> "I can't tell"
    end,
    io:format("Is ~p even? ~p~n", [Number, Type]).

head(List) ->
    [Head | _] = List,
    io:format("Head of list is: ~p~n", [Head]).

run() ->
    io:format("~p~n", [promote({ceo, female})]),
    io:format("~p~n", [promote({ceo, male})]),
    io:format("~p~n", [promote({ceo, other})]),

    io:format("~p~n", [demote({ceo, kid})]),

    Pid = spawn(?MODULE, postman, []),
    Pid ! {send, {"Joe", "Jane", "cool gift"}},

    timer:sleep(100),
    Pid ! stop,

    is_even(3),
    head([101, 201, 301]).