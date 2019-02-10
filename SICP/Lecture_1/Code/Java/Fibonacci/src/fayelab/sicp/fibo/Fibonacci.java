package fayelab.sicp.fibo;

public class Fibonacci
{
    public static int recursiveFibo(int n)
    {
        if(n < 2)
        {
            return n;
        }
        
        return recursiveFibo(n - 1) + recursiveFibo(n - 2);
    }
    
    public static int iterationFibo(int n)
    {
        if(n < 2)
        {
            return n;
        }
        
        int result = 0;
        int a = 0;
        int b = 1;
        for(int i = 2; i <= n; i++)
        {
            result = a + b;
            a = b;
            b = result;
        }
        
        return result;
    }
    
    public static int tailRecusiveFibo(int n)
    {
        return tailRecursiveFibo(n, 0, 1);
    }

    private static int tailRecursiveFibo(int count, int a, int b)
    {
        if(count == 0)
        {
            return a;
        }
        
        return tailRecursiveFibo(count - 1, b, a + b);
    }
}