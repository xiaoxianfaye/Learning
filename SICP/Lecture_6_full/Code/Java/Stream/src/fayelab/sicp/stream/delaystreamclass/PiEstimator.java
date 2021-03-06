package fayelab.sicp.stream.delaystreamclass;

import static fayelab.sicp.stream.delaystreamclass.StreamOp.*;

import java.util.function.BiFunction;

public class PiEstimator
{
    private static final int MAX_RANDOM_NUMER = 100000;

    private static Stream<Integer> randomStream()
    {
        return consStream(randomNumber(), () -> randomStream());
    }
    
    private static Stream<Boolean> cesaroStream(Stream<Integer> randomS)
    {
        return mapSuccessivePairs((Integer r1, Integer r2) -> gcd(r1, r2) == 1, randomS);
    }
    
    private static Stream<Boolean> mapSuccessivePairs(
            BiFunction<Integer, Integer, Boolean> func, Stream<Integer> s)
    {
        return consStream(func.apply(head(s), head(tail(s))), () -> mapSuccessivePairs(func, tail(tail(s))));
    }
    
    private static Stream<Double> monteCarloStream(Stream<Boolean> s, int total, int passed)
    {
        int newPassed = head(s) ? passed + 1 : passed;
        int newTotal = total + 1;
        
        System.out.println("newTotal = " + newTotal + " newPassed = " + newPassed + " p = " + 1.0 * newPassed / newTotal);
        
        return consStream(1.0 * newPassed / newTotal, () -> monteCarloStream(tail(s), newTotal, newPassed));
    }
    
    private static Stream<Double> piStream()
    {
        return mapStream(p -> doubleEquals(p, 0.0) ? 0.0 : Math.sqrt(6. / p),
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
    
    private static double streamLimit(Stream<Double> s, double tolerance)
    {
        return streamLimit(head(s), tail(s), tolerance);
    }

    private static double streamLimit(double prev, Stream<Double> s, double tolerance)
    {
        double next = head(s);
        
        System.out.println("next = " + next + " prev = " + prev);
        
        if(doubleEquals(next, prev, tolerance) && !doubleEquals(next, 2.449489742783178))
        {
            return next;
        }
        
        return streamLimit(next, tail(s), tolerance);
    }
    
    private static boolean doubleEquals(double d1, double d2)
    {
        return doubleEquals(d1, d2, 0.000000000000001);
    }
    
    private static boolean doubleEquals(double d1, double d2, double tolerance)
    {
        return Math.abs(d1 - d2) < tolerance;
    }
    
    public static void main(String[] args)
    {
        System.out.println("****** " + PiEstimator.estimate(0.0001));
        
//        Stream<Double> pis = PiEstimator.piStream();
//        System.out.println("****** 100th " + nthStream(100, pis));
//        System.out.println("****** 1000th  " + nthStream(1000, pis));
//        System.out.println("****** 10000th " + nthStream(10000, pis));
    }
}
