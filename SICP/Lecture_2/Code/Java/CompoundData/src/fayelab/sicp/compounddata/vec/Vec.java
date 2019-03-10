package fayelab.sicp.compounddata.vec;

import static java.util.Arrays.asList;

import java.util.List;

public class Vec
{
    public static List<Integer> makeVec(int xcor, int ycor)
    {
        return asList(xcor, ycor);
    }

    public static int xcor(List<Integer> vec)
    {
        return head(vec);
    }

    public static int ycor(List<Integer> vec)
    {
        return tail(vec);
    }

    public static List<Integer> add(List<Integer> vec1, List<Integer> vec2)
    {
        return makeVec(xcor(vec1) + xcor(vec2), ycor(vec1) + ycor(vec2));
    }

    public static List<Integer> scale(int c, List<Integer> vec)
    {
        return makeVec(c * xcor(vec), c * ycor(vec));
    }
    
    private static int head(List<Integer> pair)
    {
        return pair.get(0);
    }
    
    private static int tail(List<Integer> pair)
    {
        return pair.get(1);
    }
}
