package fayelab.sicp.hoproc.sqrt.fixedpoint;

import java.util.function.Function;

public class SqrtCalculator
{
    public static double sqrt(double x)
    {
        return FixedPoint.solve(y -> average(y, x / y), 1);
    }

    public static double cubeRoot(double x)
    {
        return FixedPoint.solve(y -> (x / (y * y) + 2 * y) / 3, 1);
    }
    
    private static double average(double x, double y)
    {
        return (x + y) / 2;
    }
    
    public static double sqrt2(double x)
    {
        return FixedPoint.solve(averageDump(y -> x / y), 1);
    }
    
    private static Function<Double, Double> averageDump(Function<Double, Double> func)
    {
        return y -> average(y, func.apply(y));
    }
}