package fayelab.sicp.stream.t201706;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static java.util.Arrays.asList;

public class ProceduresWithJava8Stream
{
    @SuppressWarnings("unchecked")
    static <T> Stream<T> enumTree(Object obj)
    {
        if(obj instanceof BiTuple)
        {
            BiTuple<?, ?> biTuple = (BiTuple<?, ?>)obj;
            return Stream.concat(enumTree(biTuple.getElement1()), enumTree(biTuple.getElement2()));
        }
        
        return Stream.of((T)obj);
    }

    static Stream<Integer> enumInterval(int low, int high)
    {
        return IntStream.rangeClosed(low, high).boxed();
    }
    
    public static int sumOddsSquare(BiTuple<?, ?> biTree)
    {
        Stream<Integer> s = enumTree(biTree);
        return s.filter(x -> isOdd(x))
                .map(x -> square(x))
                .reduce((x, y) -> x + y)
                .get();
    }
    
    public static List<Integer> oddFibs(int n)
    {
        List<Integer> result = new ArrayList<>();
        
        enumInterval(0, n).map(idx -> asList(idx, fib(idx)))
                          .filter(idxAndFib -> isOdd(idxAndFib.get(1)))
                          .collect(() -> result, 
                                   (acc, idxAndFib) -> acc.add(idxAndFib.get(0)),
                                   (acc1, acc2) -> acc1.addAll(acc2));

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
