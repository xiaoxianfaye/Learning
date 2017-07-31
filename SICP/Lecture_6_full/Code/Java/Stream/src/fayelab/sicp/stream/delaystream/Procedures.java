package fayelab.sicp.stream.delaystream;

import static java.util.Arrays.asList;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

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
    
    public static List<Object> integral(List<Object> s, double init, double dt)
    {
        return consStream(init, () -> addDoubleStream(doubleScale(dt, s), integral(s, init, dt)));
    }
    
    public static List<Object> funmaps(Function<Double, Double> func, double init, double dt)
    {
        return consStream(func.apply(init), () -> funmaps(func, init + dt, dt));
    }
    
    public static List<Object> integrator(Function<Double, Double> func, double init, double dt)
    {
        List<Object> ones = doubleOnes();
        List<Object> s = mapStream(func, integral(ones, init, dt));
        return integral(s, 0, dt);
    }

    public static List<Object> integralWithDelay(Supplier<List<Object>> delayS, double init, double dt)
    {
        return consStream(init, () -> {
                                          List<Object> s = delayS.get();
                                          return addDoubleStream(doubleScale(dt, s), integral(s, init, dt));
                                      });
    }
    
    public static List<Object> ys()
    {
        return integralWithDelay(() -> dys(), 1., 0.1);
    }
    
    public static List<Object> dys()
    {
        return mapStream((Double x) -> x * x, ys());
    }
    
    public static double sqrt(double d, double tolerance)
    {
        return streamLimit(sqrtStream(d), tolerance);
    }

    private static List<Object> sqrtStream(double d)
    {
        return consStream(1.0, () -> mapStream((Double ele) -> (ele + d / ele) / 2., sqrtStream(d)));
    }
    
    private static double streamLimit(List<Object> s, double tolerance)
    {
        return streamLimit(head(s), tail(s), tolerance);
    }

    private static double streamLimit(double prev, List<Object> s, double tolerance)
    {
        double next = head(s);
        if(Math.abs(next - prev) < tolerance)
        {
            return next;
        }
        return streamLimit(next, tail(s), tolerance);
    }
    
    public static List<Object> pair(List<Object> s)
    {
        return flatmap((Integer j) -> mapStream((Integer i) -> asList(i, j), enumInterval(1, j)), s);
    }
    
    public static List<Object> pair(List<Object> s, List<Object> t)
    {
        return pair(s, t, ele -> theEmptyStream());
    }

    private static <E> List<Object> pair(List<Object> s, List<Object> t, Function<E, List<Object>> func)
    {
        return appendStream(func.apply(head(t)), 
                            consStream(asList(head(s), head(t)), 
                                       () -> pair(tail(s), tail(t), accFunc(func, head(s)))));
    }

    private static <E> Function<E, List<Object>> accFunc(Function<E, List<Object>> func, E headS)
    {
        return ele -> appendStream(func.apply(ele), consStream(asList(headS, ele), () -> theEmptyStream()));
    }
    
    public static List<Object> allPairs(List<Object> s, List<Object> t)
    {
        return allPairs(s, t, eleS -> theEmptyStream(), eleT -> theEmptyStream());
    }
    
    private static <ES, ET> List<Object> allPairs(List<Object> s, List<Object> t, 
            Function<ET, List<Object>> sFunc, Function<ES, List<Object>> tFunc)
    {
        return appendStream(sFunc.apply(head(t)), 
                            appendStream(tFunc.apply(head(s)),
                                         consStream(asList(head(s), head(t)),
                                                    () -> allPairs(tail(s), tail(t), 
                                                                   accFirstFunc(sFunc, head(s)), 
                                                                   accSecondFunc(tFunc, head(t))))));
    }

    private static <ES, ET> Function<ET, List<Object>> accFirstFunc(Function<ET, List<Object>> sFunc, ES headS)
    {
        return eleT -> appendStream(sFunc.apply(eleT), consStream(asList(headS, eleT), () -> theEmptyStream()));
    }
    
    private static <ES, ET> Function<ES, List<Object>> accSecondFunc(Function<ES, List<Object>> tFunc, ET headT)
    {
        return eleS -> appendStream(tFunc.apply(eleS), consStream(asList(eleS, headT), () -> theEmptyStream()));
    }
    
    public static List<Object> reAllPairs(List<Object> s, List<Object> t)
    {
        return consStream(asList(head(s), head(t)), 
                          () -> interleave(mapStream(eleT -> asList(head(s), eleT), tail(t)),
                                           reAllPairs(tail(s), t)));
    }

    private static List<Object> interleave(List<Object> s1, List<Object> s2)
    {
        if(isEmptyStream(s1))
        {
            return s2;
        }
        
        return consStream(head(s1), () -> interleave(s2, tail(s1)));
    }
    
    public static List<Object> upPairs(List<Object> s, List<Object> t)
    {
        return consStream(asList(head(s), head(t)),
                          () -> interleave(mapStream(eleT -> asList(head(s), eleT), tail(t)),
                                           upPairs(tail(s), tail(t))));
    }
}
