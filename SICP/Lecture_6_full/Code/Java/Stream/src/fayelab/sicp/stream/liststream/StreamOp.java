package fayelab.sicp.stream.liststream;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;

import static java.util.Arrays.asList;

public class StreamOp
{
    //Constructors
    public static <T> List<T> consStream(T head, List<T> tail)
    {
        List<T> result = new ArrayList<>();
        result.add(head);
        result.addAll(tail);
        return result;
    }
    
    public static <T> List<T> theEmptyStream()
    {
        return asList();
    }
    
    //Selectors
    public static <T> T head(List<T> s)
    {
        return s.get(0);
    }
    
    public static <T> List<T> tail(List<T> s)
    {
        return s.subList(1, s.size());
    }
    
    public static <T> boolean isEmptyStream(List<T> s)
    {
        return s.isEmpty();
    }
    
    //Stream Operations    
    public static <T, R> List<R> mapStream(Function<T, R> proc, List<T> s)
    {
        if(isEmptyStream(s))
        {
            return theEmptyStream();
        }
        
        return consStream(proc.apply(head(s)), mapStream(proc, tail(s)));
    }
    
    public static <T> List<T> filterStream(Predicate<T> pred, List<T> s)
    {
        if(isEmptyStream(s))
        {
            return theEmptyStream();
        }
        
        if(pred.test(head(s)))
        {
            return consStream(head(s), filterStream(pred, tail(s)));
        }
        
        return filterStream(pred, tail(s));
    }
    
    public static <T, R> R accStream(BiFunction<T, R, R> proc, R acc, List<T> s)
    {
        if(isEmptyStream(s))
        {
            return acc;
        }
        
        return proc.apply(head(s), accStream(proc, acc, tail(s)));
    }
    
    public static <T> List<T> appendStream(List<T> s1, List<T> s2)
    {
        if(isEmptyStream(s1))
        {
            return s2;
        }
        
        return consStream(head(s1), appendStream(tail(s1), s2));
    }
    
    @SuppressWarnings("unchecked")
    public static <T> List<T> enumTree(Object obj)
    {
        if(obj instanceof BiTuple)
        {
            BiTuple<?, ?> biTuple = (BiTuple<?, ?>)obj;
            return appendStream(enumTree(biTuple.getElement1()), enumTree(biTuple.getElement2()));
        }
        
        return consStream((T)obj, theEmptyStream());
    }
    
    public static List<Integer> enumInterval(int low, int high)
    {
        if(low > high)
        {
            return theEmptyStream();
        }
        
        return consStream(low, enumInterval(low + 1, high));
    }
    
    public static <T> List<T> flatten(List<List<T>> sos)
    {
        return accStream((s1, s2) -> appendStream(s1, s2), theEmptyStream(), sos);
    }
    
    public static <T, R> List<R> flatmap(Function<T, List<R>> proc, List<T> s)
    {
        return flatten(mapStream(proc, s));
    }
    
    public static <T> boolean noneMatch(Predicate<T> pred, List<T> s)
    {
        return filterStream(pred, s).size() == 0;
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