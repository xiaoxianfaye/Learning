package fayelab.sicp.compounddata.pair;

import java.util.function.Function;

public class Pair
{
    public static <T> Function<Integer, T> makePair(T x, T y)
    {
        return index -> index == 0 ? x : y;
    }

    public static <T> T head(Function<Integer, T> pair)
    {
        return pair.apply(0);
    }

    public static <T> T tail(Function<Integer, T> pair)
    {
        return pair.apply(1);
    }
}