package fayelab.sicp.stream.delaystreamclass;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

import static java.util.Arrays.asList;
import static fayelab.sicp.stream.delaystreamclass.StreamOp.*;

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
                                      mapStream(idx -> asList(idx, fib(idx)),
                                                enumInterval(0, n))));
    }
    
    public static List<List<Integer>> primeSumPairs(int n)
    {
        return collectStream(filterStream((List<Integer> pair) -> isPrime(pair.get(0) + pair.get(1)), 
                                          flatmap(i -> mapStream(j -> asList(i, j), enumInterval(1, i - 1)), 
                                                  enumInterval(2, n))));
        
    }
    
    public static List<List<Integer>> triples(int n)
    {
        return collectStream(flatmap((Integer i) -> flatmap((Integer j) -> mapStream((Integer k) -> asList(i, j, k), 
                                                                                     enumInterval(1, j - 1)), 
                                                            enumInterval(2, i - 1)), 
                                     enumInterval(3, n)));
    }
    
    public static int secondPrime()
    {
        return head(tail(filterStream((Integer x) -> isPrime(x), enumInterval(10000, 1000000))));
    }
    
    public static Stream<Integer> noSevens()
    {
        return filterStream(x -> x % 7 != 0, integersFrom(1));
    }
    
    public static Stream<Integer> primes()
    {
        return sieve(integersFrom(2));
    }
    
    private static Stream<Integer> sieve(Stream<Integer> s)
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
    
    public static Stream<Double> integral(Stream<Double> s, double init, double dt)
    {
        return consStream(init, () -> addDoubleStream(doubleScale(dt, s), integral(s, init, dt)));
    }
    
    public static Stream<Double> funmaps(Function<Double, Double> func, double init, double dt)
    {
        return consStream(func.apply(init), () -> funmaps(func, init + dt, dt));
    }
    
    public static Stream<Double> integrator(Function<Double, Double> func, double init, double dt)
    {
        Stream<Double> ones = doubleOnes();
        Stream<Double> s = mapStream(func, integral(ones, init, dt));
        return integral(s, 0, dt);
    }

    public static Stream<Double> integralWithDelay(Supplier<Stream<Double>> delayS, double init, double dt)
    {
        return consStream(init, () -> {
                                          Stream<Double> s = delayS.get();
                                          return addDoubleStream(doubleScale(dt, s), integral(s, init, dt));
                                      });
    }
    
    public static Stream<Double> ys()
    {
        return integralWithDelay(() -> dys(), 1., 0.1);
    }
    
    public static Stream<Double> dys()
    {
        return mapStream((Double x) -> x * x, ys());
    }
    
    public static double sqrt(double d, double tolerance)
    {
        return streamLimit(sqrtStream(d), tolerance);
    }

    private static Stream<Double> sqrtStream(double d)
    {
        return consStream(1.0, () -> mapStream(ele -> (ele + d / ele) / 2., sqrtStream(d)));
    }
    
    private static double streamLimit(Stream<Double> s, double tolerance)
    {
        return streamLimit(head(s), tail(s), tolerance);
    }

    private static double streamLimit(double prev, Stream<Double> s, double tolerance)
    {
        double next = head(s);
        if(Math.abs(next - prev) < tolerance)
        {
            return next;
        }
        return streamLimit(next, tail(s), tolerance);
    }
    
    public static Stream<List<Integer>> pair(Stream<Integer> s)
    {
        return flatmap(j -> mapStream(i -> asList(i, j), enumInterval(1, j)), s);
    }
    
    public static <S, T> Stream<List<? extends Object>> pair(Stream<S> s, Stream<T> t)
    {
        return pair(s, t, ele -> theEmptyStream());
    }

    private static <S, T> Stream<List<? extends Object>> pair(Stream<S> s, Stream<T> t, 
            Function<T, Stream<List<? extends Object>>> func)
    {
        return appendStream(func.apply(head(t)), 
                            consStream(asList(head(s), head(t)), 
                                       () -> pair(tail(s), tail(t), accFunc(func, head(s)))));
    }

    private static <S, T> Function<T, Stream<List<? extends Object>>> accFunc(
            Function<T, Stream<List<? extends Object>>> func, S headS)
    {
        return ele -> appendStream(func.apply(ele), consStream(asList(headS, ele), () -> theEmptyStream()));
    }
    
    public static <S, T> Stream<List<? extends Object>> allPairs(Stream<S> s, Stream<T> t)
    {
        return allPairs(s, t, eleS -> theEmptyStream(), eleT -> theEmptyStream());
    }
    
    private static <S, T> Stream<List<? extends Object>> allPairs(Stream<S> s, Stream<T> t, 
            Function<T, Stream<List<? extends Object>>> sFunc, Function<S, Stream<List<? extends Object>>> tFunc)
    {
        return appendStream(sFunc.apply(head(t)), 
                            appendStream(tFunc.apply(head(s)),
                                         consStream(asList(head(s), head(t)),
                                                    () -> allPairs(tail(s), tail(t), 
                                                                   accFirstFunc(sFunc, head(s)), 
                                                                   accSecondFunc(tFunc, head(t))))));
    }

    private static <S, T> Function<T, Stream<List<? extends Object>>> accFirstFunc(
            Function<T, Stream<List<? extends Object>>> sFunc, S headS)
    {
        return eleT -> appendStream(sFunc.apply(eleT), consStream(asList(headS, eleT), () -> theEmptyStream()));
    }
    
    private static <S, T> Function<S, Stream<List<? extends Object>>> accSecondFunc(
            Function<S, Stream<List<? extends Object>>> tFunc, T headT)
    {
        return eleS -> appendStream(tFunc.apply(eleS), consStream(asList(eleS, headT), () -> theEmptyStream()));
    }
    
    public static <S, T> Stream<List<? extends Object>> reAllPairs(Stream<S> s, Stream<T> t)
    {
        return consStream(asList(head(s), head(t)), 
                          () -> interleave(mapStream(eleT -> asList(head(s), eleT), tail(t)),
                                           reAllPairs(tail(s), t)));
    }

    private static <S, T> Stream<List<? extends Object>> interleave(
            Stream<List<? extends Object>> s1, Stream<List<? extends Object>> s2)
    {
        if(isEmptyStream(s1))
        {
            return s2;
        }
        
        return consStream(head(s1), () -> interleave(s2, tail(s1)));
    }
    
    public static <S, T> Stream<List<? extends Object>> upPairs(Stream<S> s, Stream<T> t)
    {
        return consStream(asList(head(s), head(t)),
                          () -> interleave(mapStream(eleT -> asList(head(s), eleT), tail(t)),
                                           upPairs(tail(s), tail(t))));
    }
}
