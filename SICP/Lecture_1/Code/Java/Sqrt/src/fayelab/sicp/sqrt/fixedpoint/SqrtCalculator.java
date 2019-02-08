package fayelab.sicp.sqrt.fixedpoint;

import java.util.function.Function;

public class SqrtCalculator
{
    public static final double TOLERANCE = 0.001;
    
    public static double sqrt(double z)
    {
        Function<Double, Function<Double, Double>> func = y -> x -> (y + x / y) / 2;
        Function<Double, Double> sqrtFunc = fixedPoint(func).apply(1.0);
        return sqrtFunc.apply(z);
    }
    
    public static double cubeRoot(double z)
    {
        Function<Double, Function<Double, Double>> func = y -> x -> (x / (y * y) + 2 * y) / 3;
        Function<Double, Double> cubeRootFunc = fixedPoint(func).apply(1.0);
        return cubeRootFunc.apply(z);
    }
    
    private static Function<Double, Function<Double, Double>> fixedPoint(Function<Double, Function<Double, Double>> func)
    {
        return guess -> x -> tryOnce(guess, func, x);
    }

    private static double tryOnce(double guess, Function<Double, Function<Double, Double>> func, double x)
    {
        double nextGuess = func.apply(guess).apply(x);
        if(goodEnough(nextGuess, guess))
        {
            return nextGuess;
        }
        
        return tryOnce(nextGuess, func, x);
    }

    private static boolean goodEnough(double nextGuess, double guess)
    {
        return Math.abs(nextGuess - guess) < TOLERANCE;
    }
}
