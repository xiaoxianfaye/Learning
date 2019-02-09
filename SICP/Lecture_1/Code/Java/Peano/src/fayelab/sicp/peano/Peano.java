package fayelab.sicp.peano;

public class Peano
{
    public static int iterationAdd(int x, int y)
    {
        if(x == 0)
        {
            return y;
        }
        
        return iterationAdd(minusOne(x), plusOne(y));
    }
    
    public static int recursiveAdd(int x, int y)
    {
        if(x == 0)
        {
            return y;
        }
        
        return plusOne(recursiveAdd(minusOne(x), y));
    }
    
    private static int plusOne(int x)
    {
        return x + 1;
    }
    
    private static int minusOne(int x)
    {
        return x - 1;
    }
}