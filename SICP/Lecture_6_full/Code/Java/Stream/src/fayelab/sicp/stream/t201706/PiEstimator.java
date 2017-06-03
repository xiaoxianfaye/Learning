package fayelab.sicp.stream.t201706;

import java.util.function.BooleanSupplier;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;

public class PiEstimator
{
    private static final int MAX_RANDOM_NUMER = 100000;
    
    public static double estimate(int count)
    {
        return Math.sqrt(6 / monteCarlo(count, cesaro()));
    }

    private static double monteCarlo(int count, BooleanSupplier cesaro)
    {
        long trueCount = IntStream.range(1, count + 1)
                                  .mapToObj((c) -> cesaro.getAsBoolean())
                                  .filter((b) -> b)
                                  .count();
        return 1.0 * trueCount / count;
    }

    private static BooleanSupplier cesaro()
    {
        return () -> {
            return gcd(randomNumber(), randomNumber()) == 1;
        };
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
    
    private static DoubleStream estimate(int...counts)
    {
        return IntStream.of(counts).mapToDouble((count) -> estimate(count));
    }
    
    public static void main(String[] args)
    {
        DoubleStream pis = PiEstimator.estimate(1000, 10000, 100000, 1000000, 10000000);
        pis.forEach((pi) -> System.out.println(pi));
    }
}