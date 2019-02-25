package fayelab.sicp.hoproc.sqrt.newton;

import java.util.function.Function;

public class Newton
{
    private static double DELTA = 0.00001;

    public static double solve(Function<Double, Double> func, double guess)
    {
        return FixedPoint.solve(y -> y - func.apply(y) / derivate(func).apply(y), guess);
    }
    
    private static Function<Double, Double> derivate(Function<Double, Double> func)
    {
        return y -> (func.apply(y + DELTA) - func.apply(y)) / DELTA;
    }
}