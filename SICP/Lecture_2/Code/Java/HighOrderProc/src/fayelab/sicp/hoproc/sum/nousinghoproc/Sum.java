package fayelab.sicp.hoproc.sum.nousinghoproc;

public class Sum
{
    public static double TOLERANCE = 0.00001;

    public static double sum(double a, double b)
    {
        double sum = 0;
        for(int i = (int)Math.round(a); (i - b) <= TOLERANCE; i++)
        {
            sum = sum + i;
        }
        return sum;
    }

    public static double sum_r(double a, double b)
    {
        if(a - b > TOLERANCE)
        {
            return 0;
        }
        
        return a + sum_r(a + 1, b);
    }

    public static double sum_tr(double a, double b)
    {
        return sum_tr(a, b, 0);
    }

    private static double sum_tr(double a, double b, double acc)
    {
        if(a - b > TOLERANCE)
        {
            return acc;
        }
        
        return sum_tr(a + 1, b, a + acc);
    }

    public static double sumSquare(double a, double b)
    {
        double sum = 0;
        for(int i = (int)Math.round(a); (i - b) <= TOLERANCE; i++)
        {
            sum = sum + i * i;
        }
        return sum;
    }

    public static double sumSquare_r(double a, double b)
    {
        if(a - b > TOLERANCE)
        {
            return 0;
        }
        
        return a * a + sumSquare_r(a + 1, b);
    }

    public static double sumSquare_tr(double a, double b)
    {
        return sumSquare_tr(a, b, 0);
    }

    private static double sumSquare_tr(double a, double b, double acc)
    {
        if(a - b > TOLERANCE)
        {
            return acc;
        }
        
        return sumSquare_tr(a + 1, b, acc + a * a);
    }

    public static double sumPi(double a, double b)
    {
        double sum = 0;
        for(int i = (int)Math.round(a); (i - b) <= TOLERANCE; i+=4)
        {
            sum = sum + 1.0 / (i * (i + 2.0));
        }
        return sum;
    }

    public static double sumPi_r(double a, double b)
    {
        if(a - b > TOLERANCE)
        {
            return 0;
        }
        
        return 1.0 / (a * (a + 2.0)) + sumPi_r(a + 4, b);
    }

    public static double sumPi_tr(double a, double b)
    {
        return sumPi_tr(a, b, 0);
    }

    private static double sumPi_tr(double a, double b, double acc)
    {
        if(a - b > TOLERANCE)
        {
            return acc;
        }
        
        return sumPi_tr(a + 4, b, acc + 1.0 / (a * (a + 2.0)));
    }
}