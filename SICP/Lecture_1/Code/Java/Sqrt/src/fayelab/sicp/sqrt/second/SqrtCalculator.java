package fayelab.sicp.sqrt.second;

public class SqrtCalculator
{
    public static final double TOLERANCE = 0.001;
    
    public static double sqrt(double x)
    {
        return tryOnce(1.0, x);
    }

    private static double tryOnce(double guess, double x)
    {
        double nextGuess = average(guess, x / guess);
        if(goodEnough(nextGuess, guess))
        {
            return nextGuess;
        }
        
        return tryOnce(nextGuess, x);
    }

    private static boolean goodEnough(double nextGuess, double guess)
    {
        return Math.abs(nextGuess - guess) < TOLERANCE;
    }

    private static double average(double x, double y)
    {
        return (x + y) / 2;
    }
}
