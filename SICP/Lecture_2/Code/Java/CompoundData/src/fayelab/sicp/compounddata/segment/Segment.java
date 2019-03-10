package fayelab.sicp.compounddata.segment;

import static java.util.Arrays.asList;

import java.util.List;

public class Segment
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

    public static List<List<Integer>> makeSegment(List<Integer> startVec, List<Integer> endVec)
    {
        return asList(startVec, endVec);
    }

    public static List<Integer> segmentStart(List<List<Integer>> segment)
    {
        return head(segment);
    }

    public static List<Integer> segmentEnd(List<List<Integer>> segment)
    {
        return tail(segment);
    }

    public static List<Integer> middlePoint(List<List<Integer>> segment)
    {
        List<Integer> startVec = segmentStart(segment);
        List<Integer> endVec = segmentEnd(segment);
        
        return makeVec(average(xcor(startVec), xcor(endVec)),
                       average(ycor(startVec), ycor(endVec)));
    }

    public static int length(List<List<Integer>> segment)
    {
        List<Integer> startVec = segmentStart(segment);
        List<Integer> endVec = segmentEnd(segment);

        int xlength = xcor(startVec) - xcor(endVec);
        int ylength = ycor(startVec) - ycor(endVec);
        return (int)Math.sqrt(square(xlength) + square(ylength));
    }
    
    private static <T> T head(List<T> pair)
    {
        return pair.get(0);
    }
    
    private static <T> T tail(List<T> pair)
    {
        return pair.get(1);
    }

    public static int average(int x, int y)
    {
        return (int)((x + y) / 2);
    }

    public static int square(int x)
    {
        return x * x;
    }
}