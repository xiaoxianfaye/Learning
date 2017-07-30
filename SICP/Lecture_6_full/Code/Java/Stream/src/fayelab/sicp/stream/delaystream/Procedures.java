package fayelab.sicp.stream.delaystream;

import static java.util.Arrays.asList;

import java.util.ArrayList;
import java.util.List;

import static fayelab.sicp.stream.delaystream.StreamOp.*;

public class Procedures
{
    public static <T1, T2> int sumOddsSquare(BiTuple<T1, T2> biTree)
    {
        return accStream((Integer x, Integer y) -> x + y,
                         0,
                         mapStream((Integer x) -> square(x),
                                   filterStream((Integer x) -> isOdd(x),
                                                enumTree(biTree))));
    }
    
    public static List<Integer> oddFibs(int n)
    {
        return accStream((List<Integer> idxAndFib, List<Integer> acc) -> glue(idxAndFib.get(0), acc), 
                         new ArrayList<>(),
                         filterStream((List<Integer> idxAndFib) -> isOdd(idxAndFib.get(1)),
                                      mapStream((Integer idx) -> asList(idx, fib(idx)),
                                                enumInterval(0, n))));
    }
    
    public static List<List<Integer>> primeSumPairs(int n)
    {
        return collectStream(filterStream((List<Integer> pair) -> isPrime(pair.get(0) + pair.get(1)),
                                          flatmap((Integer i) -> mapStream(j -> asList(i, j),
                                                                           enumInterval(1, i - 1)),
                                                  enumInterval(2, n))));
    }
    
    public static List<List<Integer>> triples(int n)
    {
        return collectStream(flatmap((Integer i) -> flatmap((Integer j) -> mapStream(k -> asList(i, j, k), 
                                                                                     enumInterval(1, j - 1)), 
                                                            enumInterval(2, i - 1)), 
                                     enumInterval(3, n)));
    }
    
    public static int secondPrime()
    {
        return head(tail(filterStream((Integer x) -> isPrime(x), enumInterval(10000, 1000000))));
    }
    
    public static List<Object> noSevens()
    {
        return filterStream((Integer x) -> x % 7 != 0, integersFrom(1));
    }
    
    public static List<Object> primes()
    {
        return sieve(integersFrom(2));
    }
    
    private static List<Object> sieve(List<Object> s)
    {
        return consStream(head(s), () -> sieve(filterStream((Integer n) -> n % (Integer)head(s) != 0, tail(s))));
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
        return noneMatch((Integer i) -> n % i == 0, enumInterval(2, n - 1));
    }
}
