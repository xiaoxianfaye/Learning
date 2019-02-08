package fayelab.sicp.sqrt.first;

public class SqrtCalculator
{
    public static final double TOLERANCE = 0.001;
    
    public static double sqrt(double x)
    {
        return tryOnce(1.0, x);
    }

    private static double tryOnce(double guess, double x)
    {
        if(goodEnough(guess, x))
        {
            return guess;
        }
        
        return tryOnce(improve(guess, x), x);
    }

    private static boolean goodEnough(double guess, double x)
    {
        return Math.abs(square(guess) - x) < TOLERANCE;
    }

    private static double improve(double guess, double x)
    {
        return average(guess, x / guess);
    }

    private static double square(double x)
    {
        return x * x;
    }

    private static double average(double x, double y)
    {
        return (x + y) / 2;
    }
}
