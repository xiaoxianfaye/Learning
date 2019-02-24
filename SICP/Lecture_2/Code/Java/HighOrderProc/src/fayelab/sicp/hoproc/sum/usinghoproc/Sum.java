package fayelab.sicp.hoproc.sum.usinghoproc;

import java.util.function.Function;

public class Sum
{
    public static double TOLERANCE = 0.00001;

    public static double sum(Function<Double, Double> term, double a, Function<Double, Double> next, double b)
    {
        double sum = 0;
        for(int i = (int)Math.round(a); (i - b) <= TOLERANCE; i=(int)Math.round(next.apply((double)i)))
        {
            sum = sum + term.apply((double)i);
        }
        return sum;
    }
    
    public static double sum_r(Function<Double, Double> term, double a, Function<Double, Double> next, double b)
    {
        if(a - b > TOLERANCE)
        {
            return 0;
        }
        
        return term.apply(a) + sum_r(term, next.apply(a), next, b);
    }
    
    public static double sum_tr(Function<Double, Double> term, double a, Function<Double, Double> next, double b)
    {
        return sum_tr(term, a, next, b, 0);
    }
    
    private static double sum_tr(Function<Double, Double> term, double a, Function<Double, Double> next, double b, double acc)
    {
        if(a - b > TOLERANCE)
        {
            return acc;
        }
        
        return sum_tr(term, next.apply(a), next, b, acc + term.apply(a));
    }

    public static double sum(double a, double b)
    {
        return sum(x -> x, a, x -> x + 1, b);
    }
    
    public static double sumSquare(double a, double b)
    {
        return sum(x -> x * x, a, x -> x + 1, b);
    }
    
    public static double sumPi(double a, double b)
    {
        return sum(x -> 1.0 / (x * (x + 2.0)), a, x -> x + 4, b);
    }
    
    public static double sumPrimitive_r(double a, double b)
    {
        return sum_r(x -> x, a, x -> x + 1, b);
    }
    
    public static double sumSquare_r(double a, double b)
    {
        return sum_r(x -> x * x, a, x -> x + 1, b);
    }
    
    public static double sumPi_r(double a, double b)
    {
        return sum_r(x -> 1.0 / (x * (x + 2.0)), a, x -> x + 4, b);
    }
    
    public static double sumPrimitive_tr(double a, double b)
    {
        return sum_tr(x -> x, a, x -> x + 1, b);
    }
    
    public static double sumSquare_tr(double a, double b)
    {
        return sum_tr(x -> x * x, a, x -> x + 1, b);
    }
    
    public static double sumPi_tr(double a, double b)
    {
        return sum_tr(x -> 1.0 / (x * (x + 2.0)), a, x -> x + 4, b);
    }
}