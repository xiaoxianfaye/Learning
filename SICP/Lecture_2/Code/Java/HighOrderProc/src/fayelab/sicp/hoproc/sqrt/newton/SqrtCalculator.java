package fayelab.sicp.hoproc.sqrt.newton;

public class SqrtCalculator
{
    public static double sqrt(double x)
    {
        return Newton.solve(y -> x - y * y, 1);
    }

    public static double cubeRoot(double x)
    {
        return Newton.solve(y -> x - y * y * y, 1);
    }
}