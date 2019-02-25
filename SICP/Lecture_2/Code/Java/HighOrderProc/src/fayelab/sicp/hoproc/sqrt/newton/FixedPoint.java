package fayelab.sicp.hoproc.sqrt.newton;

import java.util.function.Function;

public class FixedPoint
{
    public static final double TOLERANCE = 0.00001;

    public static double solve(Function<Double, Double> func, double guess)
    {
        return fixedPoint_tr(func, guess, func.apply(guess));
    }

    private static double fixedPoint_tr(Function<Double, Double> func, double oldGuess, double newGuess)
    {
        if(closeEnough(oldGuess, newGuess))
        {
            return newGuess;
        }
        
        return fixedPoint_tr(func, newGuess, func.apply(newGuess));
    }

    private static boolean closeEnough(double oldGuess, double newGuess)
    {
        return Math.abs(newGuess - oldGuess) < TOLERANCE;
    } 
}