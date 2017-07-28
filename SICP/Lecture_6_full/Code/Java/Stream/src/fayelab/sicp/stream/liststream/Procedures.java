package fayelab.sicp.stream.liststream;

import static fayelab.sicp.stream.liststream.StreamOp.*;
import static java.util.Arrays.asList;

import java.util.ArrayList;
import java.util.List;

public class Procedures
{
    public static <T1, T2> int sumOddsSquare(BiTuple<T1, T2> biTree)
    {
        return accStream((x, y) -> x + y, 
                         0, 
                         mapStream(x -> square(x),
                                   filterStream((Integer x) -> isOdd(x),
                                                enumTree(biTree))));
    }
    
    public static List<Integer> oddFibs(int n)
    {
        return accStream((idxAndFib, acc) -> glue(idxAndFib.get(0), acc), 
                         new ArrayList<>(),
                         filterStream(idxAndFib -> isOdd(idxAndFib.get(1)),
                                      mapStream((Integer idx) -> asList(idx, fib(idx)),
                                                enumInterval(0, n))));
    }
    
    public static List<List<Integer>> primeSumPairs(int n)
    {
        return filterStream((List<Integer> pair) -> isPrime(pair.get(0) + pair.get(1)),
                            flatmap(i -> mapStream(j -> asList(i, j),
                                                   enumInterval(1, i - 1)), 
                                    enumInterval(2, n)));
    }
    
    public static List<List<Integer>> triples(int n)
    {
        return flatmap(i -> flatmap(j -> mapStream(k -> asList(i, j, k), enumInterval(1, j - 1)), 
                                    enumInterval(2, i - 1)), 
                       enumInterval(3, n));
    }

    private static boolean isOdd(int n)
    {
        return n % 2 == 1;
    }
    
    private static int square(int n)
    {
        return n * n;
    }
    
    private static int fib(int n)
    {
        int a = 0;
        int b = 1;
        int s = 0;
        
        if(n == 0)
        {
            return a;
        }
        
        if(n == 1)
        {
            return b;
        }
        
        for(int i = 0; i <= n - 2; i++)
        {
            s = a + b;
            a = b;
            b = s;
        }
        
        return s;
    }
    
    private static <T> List<T> glue(T newHead, List<T> list)
    {
        List<T> result = new ArrayList<>();
        result.add(newHead);
        result.addAll(list);
        return result;
    }
    
    private static boolean isPrime(int n)
    {
        return noneMatch(i -> n % i == 0, enumInterval(2, n - 1));
    }
}
