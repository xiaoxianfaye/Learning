package fayelab.sicp.stream.delaystream;

import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import java.util.ArrayList;

import static java.util.Arrays.asList;

public class StreamOp
{
    //Constructors
    public static <T> List<Object> consStream(T head, Supplier<List<Object>> tail)
    {
        return asList(head, tail);
    }
    
    public static List<Object> theEmptyStream()
    {
        return asList();
    }
    
    //Selectors
    @SuppressWarnings("unchecked")
    public static <T> T head(List<Object> s)
    {
        return (T)s.get(0);
    }
    
    @SuppressWarnings("unchecked")
    public static List<Object> tail(List<Object> s)
    {
        return ((Supplier<List<Object>>)s.get(1)).get();
    }
    
    public static boolean isEmptyStream(List<Object> s)
    {
        return s.isEmpty();
    }
    
    public static <T, R> List<Object> mapStream(Function<T, R> proc, List<Object> s)
    {
        if(isEmptyStream(s))
        {
            return theEmptyStream();
        }
        
        return consStream(proc.apply(head(s)), () -> mapStream(proc, tail(s)));
    }
    
    public static <T> List<Object> filterStream(Predicate<T> pred, List<Object> s)
    {
        if(isEmptyStream(s))
        {
            return theEmptyStream();
        }
        
        if(pred.test(head(s)))
        {
            return consStream(head(s), () -> filterStream(pred, tail(s)));
        }
        
        return filterStream(pred, tail(s));
    }
    
    public static <T, R> R accStream(BiFunction<T, R, R> proc, R acc, List<Object> s)
    {
        if(isEmptyStream(s))
        {
            return acc;
        }
        
        return proc.apply(head(s), accStream(proc, acc, tail(s)));
    }
    
    public static List<Object> appendStream(List<Object> s1, List<Object> s2)
    {
        if(isEmptyStream(s1))
        {
            return s2;
        }
        
        return consStream(head(s1), () -> appendStream(tail(s1), s2));
    }
    
    public static List<Object> enumTree(Object obj)
    {
        if(obj instanceof BiTuple)
        {
            BiTuple<?, ?> biTuple = (BiTuple<?, ?>)obj;
            return appendStream(enumTree(biTuple.getElement1()), enumTree(biTuple.getElement2()));
        }
        
        return consStream(obj, () -> theEmptyStream());
    }
    
    public static List<Object> enumInterval(int low, int high)
    {
        if(low > high)
        {
            return theEmptyStream();
        }
        
        return consStream(low, () -> enumInterval(low + 1, high));
    }
    
//    public static List<Object> flatten(List<Object> sos)
//    {
//        return accStream((List<Object> s1, List<Object> s2) -> appendStream(s1, s2), theEmptyStream(), sos);
//    }
    
    public static List<Object> flatten(List<Object> sos)
    {
        if(isEmptyStream(sos))
        {
            return theEmptyStream();
        }
        
        return flatten(head(sos), tail(sos));
    }
    
    private static List<Object> flatten(List<Object> headS, List<Object> sos)
    {
        if(isEmptyStream(headS))
        {
            return flatten(sos);
        }
        
        return consStream(head(headS), () -> flatten(tail(headS), sos));
    }

    public static <T> List<Object> flatmap(Function<T, List<Object>> proc, List<Object> s)
    {
        return flatten(mapStream(proc, s));
    }
    
    public static <T> boolean noneMatch(Predicate<T> pred, List<Object> s)
    {
        return filterStream(pred, s).size() == 0;
    }
    
    public static Object nthStream(int index, List<Object> s)
    {
        if(isEmptyStream(s))
        {
            throw new RuntimeException("This index does not exist.");
        }
        
        return index == 0 ? head(s) : nthStream(index - 1, tail(s));
    }
    
    public static void printStream(List<Object> s)
    {
        if(isEmptyStream(s))
        {
            System.out.println("Done.\n");
            return;
        }
        
        System.out.println(head(s).toString());
        printStream(tail(s));        
    }
    
    public static void printStreamLimit(int num, List<Object> s)
    {
        if(isEmptyStream(s))
        {
            System.out.println("Done.\n");
            return;
        }
        
        if(num == 0)
        {
            System.out.println("ok.\n");
            return;
        }
        
        System.out.println(head(s).toString());
        printStreamLimit(num - 1,tail(s));
    }
    
    public static List<Object> integersFrom(int n)
    {
        return consStream(n, () -> integersFrom(n + 1));
    }
    
    public static List<Object> addStream(List<Object> s1, List<Object> s2)
    {
        if(isEmptyStream(s1) & isEmptyStream(s2))
        {
            return theEmptyStream();
        }
        
        if(isEmptyStream(s1))
        {
            return s2;
        }
        
        if(isEmptyStream(s2))
        {
            return s1;
        }
        
        return consStream((Integer)head(s1) + (Integer)head(s2), () -> addStream(tail(s1), tail(s2)));
    }
    
    public static List<Object> scale(int c, List<Object> s)
    {
        if(isEmptyStream(s))
        {
            return theEmptyStream();
        }
        
        return consStream(c * (Integer)head(s), () -> scale(c, tail(s)));
    }
    
    public static List<Object> ones()
    {
        return consStream(1, () -> ones());
    }
    
    public static List<Object> integers()
    {
        return consStream(1, () -> addStream(integers(), ones()));
    }
    
    public static List<Object> fibs(int n1, int n2)
    {
        return consStream(n1, () -> fibs(n2, n1 + n2));
    }
    
    public static List<Object> fibs()
    {
        return consStream(0, () -> consStream(1, () -> addStream(fibs(), tail(fibs()))));
    }
    
    public static List<Object> doubleOnes()
    {
        return consStream(1., () -> doubleOnes());
    }
    
    public static List<Object> doubles()
    {
        return consStream(1., () -> addDoubleStream(doubles(), doubleOnes()));
    }
    
    public static List<Object> doubles(double low, double high)
    {
        if(low > high)
        {
            return theEmptyStream();
        }
        
        return consStream(low, () -> doubles(low + 1., high));
    }
    
    public static List<Object> doublesFrom(double d)
    {
        return consStream(d, () -> doublesFrom(d + 1.));
    }
    
    public static List<Object> addDoubleStream(List<Object> s1, List<Object> s2)
    {
        if(isEmptyStream(s1) & isEmptyStream(s2))
        {
            return theEmptyStream();
        }
        
        if(isEmptyStream(s1))
        {
            return s2;
        }
        
        if(isEmptyStream(s2))
        {
            return s1;
        }
        
        return consStream((Double)head(s1) + (Double)head(s2), () -> addDoubleStream(tail(s1), tail(s2)));
    }
    
    public static List<Object> doubleScale(double c, List<Object> s)
    {
        if(isEmptyStream(s))
        {
            return theEmptyStream();
        }
        
        return consStream(c * (Double)head(s), () -> doubleScale(c, tail(s)));
    }
    
    //Auxiliary methods
    @SafeVarargs
    static <T> List<Object> listToStream(T...elements)
    {
        return listToStream(new ArrayList<>(asList(elements)));
    }
    
    private static <T> List<Object> listToStream(List<T> list)
    {
        if(list.isEmpty())
        {
            return theEmptyStream();
        }
        
        T head = list.remove(0);
        return consStream(head, () -> listToStream(list));
    }
    
    static <T> List<T> collectStream(List<Object> s)
    {
        List<T> result = new ArrayList<>();
        
        if(isEmptyStream(s))
        {
            return result;
        }
        
        result.add(head(s));
        result.addAll(collectStream(tail(s)));
        
        return result;
    }
    
    public static <T> List<T> collectStreamLimit(int num, List<Object> s)
    {
        List<T> result = new ArrayList<>();
        if(isEmptyStream(s))
        {
            return result;
        }
        
        collectStreamLimit(num, s, result);
        return result;        
    }
    
    private static <T> void collectStreamLimit(int num, List<Object> s, List<T> result)
    {
        if(num == 0)
        {
            return;
        }
        
        result.add(head(s));
        collectStreamLimit(num - 1, tail(s), result);
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
