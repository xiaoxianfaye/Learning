I = integers_from(1)
<=> % 根据integers_from定义替换integers_from
cons_stream(1, fun() -> integers_from(2) end)
{
    Ref = make_ref() => #Ref<0.0.0.93>,
    F => 
        fun() ->
            case get(Ref) of % closure Ref #Ref<0.0.0.93>
                undefined ->
                    Res = (fun() -> integers_from(2) end)()
                        => integers_from(2),
                    put(Ref, Res),
                    Res;
                R ->
                    R
            end
        end
}


nth_stream(1, I) <=> head(I) => 1


% nth_stream(2, I) first
nth_stream(2, I) <=> nth_stream(1, tail(I))

tail(I) <=> F()
<=>
(
    fun() ->
        case get(Ref) of % closure Ref #Ref<0.0.0.93>
            undefined ->
                Res = integers_from(2),
                put(Ref, Res),
                Res;
            R ->
                R
        end
    end
)()
<=>
case get(Ref) of % closure Ref #Ref<0.0.0.93>
    undefined ->
        Res = integers_from(2),
        put(Ref, Res),
        Res;
    R ->
        R
end
<=> % undefined branch
Res = integers_from(2),
put(#Ref<0.0.0.93>, Res),
Res;

integers_from(2)
<=> % 根据integers_from定义替换integers_from
cons_stream(2, fun() -> integers_from(3) end)
{
    make_ref() => Ref #Ref<0.0.0.104>
    F => 
        fun() ->
            case get(Ref) of % closure Ref #Ref<0.0.0.104>
                undefined ->
                    Res = (fun() -> integers_from(3) end)(),
                    put(Ref, Res),
                    Res;
                R ->
                    R
            end
        end
}

nth_stream(1, tail(I))
<=>
nth_stream(1, integers_from(2))
<=>
head(integers_from(2)) => 2


% nth_stream(2, I) again
nth_stream(2, I) <=> nth_stream(1, tail(I))

tail(I) <=> F()
<=>
(
    fun() ->
        case get(Ref) of % closure Ref #Ref<0.0.0.93>
            undefined ->
                Res = integers_from(2),
                put(Ref, Res),
                Res;
            R ->
                R
        end
    end
)()
<=>
case get(Ref) of % closure Ref #Ref<0.0.0.93>
    undefined ->
        Res = integers_from(2),
        put(Ref, Res),
        Res;
    R ->
        R
end
<=> % R branch
integers_from(2)

nth_stream(1, tail(I))
<=>
nth_stream(1, integers_from(2))
<=>
head(integers_from(2)) => 2
