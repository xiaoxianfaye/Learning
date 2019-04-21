-module(map).
-compile(export_all).

write(String, Value) ->
    io:format("~p = ~p~n", [String, Value]).

run() ->
    M1 = #{name => "Joe Doe", age => 25},

    write("Map", M1),
    write("Name", maps:get(name, M1)),
    write("Degree", maps:get(degree, M1, defaultdegree)),

    Keyname = randomkey,
    case maps:find(Keyname, M1) of
        {ok, Value} ->
            write("Found value", Value);
        error ->
            write("No value found for key", Keyname)
    end.