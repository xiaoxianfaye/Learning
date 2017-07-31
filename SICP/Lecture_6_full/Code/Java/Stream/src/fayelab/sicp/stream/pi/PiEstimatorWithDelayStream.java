package fayelab.sicp.stream.pi;

import java.util.List;
import java.util.function.BiFunction;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;

import static fayelab.sicp.stream.delaystream.StreamOp.*;

public class PiEstimatorWithDelayStream
{
    private static final int MAX_RANDOM_NUMER = 100000;
    
    private static List<Object> randomStream()
    {
        return consStream(randomNumber(), () -> randomStream());
    }
    
    private static List<Object> cesaroStream(List<Object> randomS)
    {
        return mapSuccessivePairs((Integer r1, Integer r2) -> gcd(r1, r2) == 1, randomS);
    }
    
    private static <T, U, R> List<Object> mapSuccessivePairs(BiFunction<T, U, R> func, List<Object> s)
    {
        return consStream(func.apply(head(s), head(tail(s))), () -> mapSuccessivePairs(func, tail(tail(s))));
    }
    
    private static List<Object> monteCarloStream(List<Object> s, int total, int passed)
    {
        int newPassed = (Boolean)head(s) == true ? passed + 1 : passed;
        int newTotal = total + 1;
        return consStream(newPassed / newTotal * 1.0, () -> monteCarloStream(tail(s), newTotal, newPassed));
    }
    
    static List<Object> piStream()
    {
        return mapStream((Double p) -> p == 0.0 ? 0.0 : Math.sqrt(6. / p),
                         monteCarloStream(cesaroStream(randomStream()), 0, 0));
    }
    
    public static double estimate(double tolerance)
    {
        return streamLimit(piStream(), tolerance);
    }
    
    private static int randomNumber()
    {
        return (int)(Math.random() * MAX_RANDOM_NUMER) + 1;
    }
    
    private static int gcd(int x, int y)
    {
        if(y == 0)
        {
            return x;
        }
        
        return gcd(y, x % y);
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
    
    private static DoubleStream estimate(int...counts)
    {
        return IntStream.of(counts).mapToDouble(count -> estimate(count));
    }
    
    public static void main(String[] args)
    {
        System.out.println(PiEstimatorWithDelayStream.estimate(0.001));
        
        List<Object> pis = piStream();
        System.out.println(nthStream(100, pis));
        System.out.println(nthStream(1000, pis));
        System.out.println(nthStream(10000, pis));
        
//        DoubleStream pis = PiEstimatorWithDelayStream.estimate(1000, 10000, 100000, 1000000, 10000000);
//        pis.forEach((pi) -> System.out.println(pi));
    }
}
