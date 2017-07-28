package fayelab.sicp.stream.nostream;

import java.util.ArrayList;
import java.util.List;

import static java.util.Arrays.asList;

public class Procedures
{
    public static int sumOddsSquare(Object obj)
    {
        if(obj instanceof BiTuple<?, ?>)
        {
            return sumOddsSquareForBiTuple((BiTuple<?, ?>)obj);
        }
        
        return sumOddsSquareForInteger((Integer)obj);
    }

    private static int sumOddsSquareForBiTuple(BiTuple<?, ?> biTuple)
    {
        return sumOddsSquare(biTuple.getElement1()) + sumOddsSquare(biTuple.getElement2());
    }

    private static int sumOddsSquareForInteger(Integer integer)
    {
        int n = integer.intValue();
        
        if(isOdd(n))
        {
            return square(n);
        }
        
        return 0;
    }
    
    public static List<Integer> oddFibs(int n)
    {
        List<Integer> result = new ArrayList<>();
        
        for(int i = 0; i <= n; i++)
        {
            if(isOdd(fib(i)))
            {
                result.add(i);
            }
        }
        
        return result;
    }
    
    public static List<List<Integer>> primeSumPairs(int n)
    {
        List<List<Integer>> result = new ArrayList<>();
        
        for(int i = 2; i <= n; i++)
        {
            for(int j = 1; j < i; j++)
            {
                if(isPrime(i + j))
                {
                    result.add(asList(i, j));
                }
            }
        }
        
        return result;
    }
    
    public static List<List<Integer>> triples(int n)
    {
        List<List<Integer>> result = new ArrayList<>();
        
        for(int i = 3; i <= n; i++)
        {
            for(int j = 2; j < i; j++)
            {
                for(int k = 1; k < j; k++)
                {
                    result.add(asList(i, j, k));
                }
            }
        }
        
        return result;
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
    
    private static boolean isPrime(int n)
    {
        for(int i = 2; i < n; i++)
        {
            if(n % i == 0)
            {
                return false;
            }
        }
        
        return true;
    }

    public static <T1, T2> BiTuple<T1, T2> biTuple(T1 element1, T2 element2)
    {
        return new BiTuple<>(element1, element2);
    }
    
    static class BiTuple<T1, T2>
    {
        private T1 element1;
        private T2 element2;
        
        public BiTuple(T1 element1, T2 element2)
        {
            this.element1 = element1;
            this.element2 = element2;
        }
    
        public T1 getElement1()
        {
            return element1;
        }
    
        public T2 getElement2()
        {
            return element2;
        }
    }
}
