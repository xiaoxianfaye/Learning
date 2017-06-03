package fayelab.sicp.stream.t201706;

import java.util.List;

public class ProceduresWithJava8Stream
{
    public static <E1, E2> int sumOddsSquare(BiTuple<E1, E2> biTree)
    {
        return 0;
    }
    
    public static List<Integer> oddFibs(int n)
    {
        return null;
    }
    
    private static boolean isOdd(int n)
    {
        return n % 2 == 1;
    }
    
    private static int square(int n)
    {
        return n * n;
    }
    
    private static int fib(int n)
    {
        int a = 0;
        int b = 1;
        int s = 0;
        
        if(n == 0)
        {
            return a;
        }
        
        if(n == 1)
        {
            return b;
        }
        
        for(int i = 0; i <= n - 2; i++)
        {
            s = a + b;
            a = b;
            b = s;
        }
        
        return s;
    }

    public static <E1, E2> BiTuple<E1, E2> biTuple(E1 element1, E2 element2)
    {
        return new BiTuple<>(element1, element2);
    }

    static class BiTuple<E1, E2>
    {
        private E1 element1;
        private E2 element2;
        
        public BiTuple(E1 element1, E2 element2)
        {
            this.element1 = element1;
            this.element2 = element2;
        }
    
        public E1 getElement1()
        {
            return element1;
        }
    
        public E2 getElement2()
        {
            return element2;
        }
    }
}
