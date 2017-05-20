S0...   T0...   Fun <=> fun(E) -> the_empty_stream() end

Fun(head(T)) <=> (fun(E) -> the_empty_stream())(T0)
             <=> the_empty_stream()
             
append_stream(
    the_empty_stream(),
    stream({S0, T0})
)

stream({S0, T0})


S1...   T1...   Fun <=> acc_fun(fun(E) -> the_empty_stream() end, S0))

Fun(head(T)) <=> (acc_fun(fun(E) -> the_empty_stream() end, S0))(T1)
             <=> append_stream(
                    (fun(E) -> the_empty_stream() end)(T1),
                    stream({S0, T1})
                 )
             <=> append_stream(
                    the_empty_stream(),
                    stream({S0, T1})
                 )
             <=> stream({S0, T1})
             
append_stream(
    stream({S0, T1})
    stream({S1, T1})
)            

stream({S0, T0}, {S0, T1}, {S1, T1})


S2...   T2...   Fun <=> acc_fun(acc_fun(fun(E) -> the_empty_stream() end, S0), S1)

Fun(head(T)) <=> (acc_fun(acc_fun(fun(E) -> the_empty_stream() end, S0), S1))(T2)
             <=> append_stream(
                    (acc_fun(fun(E) -> the_empty_stream() end, S0))(T2),
                    stream(S1, T2)
                 )
             <=> append_stream(
                    append_stream(
                        (fun(E) -> the_empty_stream())(T2),
                        stream(S0, T2)
                    )
                    stream(S1, T2)
                 )
             <=> stream({S0, T2}, {S1, T2})
             
append_stream(
    stream({S0, T2}, {S1, T2})
    stream({S2, T2})
)

stream({S0, T0}, {S0, T1}, {S1, T1}, {S0, T2}, {S1, T2}, {S2, T2})