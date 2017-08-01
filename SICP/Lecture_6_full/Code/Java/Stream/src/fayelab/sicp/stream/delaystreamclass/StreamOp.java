package fayelab.sicp.stream.delaystreamclass;

import java.util.List;
import java.util.ArrayList;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import static java.util.Arrays.asList;

public class StreamOp
{
    //Constructors
    public static <T> Stream<T> consStream(T head, Supplier<Stream<T>> tail)
    {
        return new Stream<>(head, tail);
    }
    
    public static <T> Stream<T> theEmptyStream()
    {
        return new Stream<>(null, null);
    }
    
    //Selectors
    public static <T> T head(Stream<T> s)
    {
        return s.head();
    }
    
    public static <T> Stream<T> tail(Stream<T> s)
    {
        return s.tail();
    }
    
    public static <T> boolean isEmptyStream(Stream<T> s)
    {
        return s.isEmpty();
    }
    
    //Stream Operations
    public static <T, R> Stream<R> mapStream(Function<T, R> proc, Stream<T> s)
    {
        if(s.isEmpty())
        {
            return theEmptyStream();
        }
        
        return consStream(proc.apply(head(s)), () -> mapStream(proc, tail(s)));
    }
    
    public static <T> Stream<T> filterStream(Predicate<T> pred, Stream<T> s)
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
    
    public static <T, R> R accStream(BiFunction<T, R, R> proc, R acc, Stream<T> s)
    {
        if(isEmptyStream(s))
        {
            return acc;
        }
        
        return proc.apply(head(s), accStream(proc, acc, tail(s)));
    }
    
    public static <T> Stream<T> appendStream(Stream<T> s1, Stream<T> s2)
    {
        if(isEmptyStream(s1))
        {
            return s2;
        }
        
        return consStream(head(s1), () -> appendStream(tail(s1), s2));
    }
    
    @SuppressWarnings("unchecked")
    public static <T> Stream<T> enumTree(Object obj)
    {
        if(obj instanceof BiTuple)
        {
            BiTuple<?, ?> biTuple = (BiTuple<?, ?>)obj;
            return appendStream(enumTree(biTuple.getElement1()), enumTree(biTuple.getElement2()));
        }
        
        return consStream((T)obj, () -> theEmptyStream());
    }
    
    public static Stream<Integer> enumInterval(int low, int high)
    {
        if(low > high)
        {
            return theEmptyStream();
        }
        
        return consStream(low, () -> enumInterval(low + 1, high));
    }
    
//    public static <T> Stream<T> flatten(Stream<Stream<T>> sos)
//    {
//        return accStream((s1, s2) -> appendStream(s1, s2), theEmptyStream(), sos);
//    }
    
    public static <T> Stream<T> flatten(Stream<Stream<T>> sos)
    {
        if(isEmptyStream(sos))
        {
            return theEmptyStream();
        }
        
        return flatten(head(sos), tail(sos));
    }
    
    private static <T> Stream<T> flatten(Stream<T> headS, Stream<Stream<T>> sos)
    {
        if(isEmptyStream(headS))
        {
            return flatten(sos);
        }
        
        return consStream(head(headS), () -> flatten(tail(headS), sos));
    }

    public static <T, R> Stream<R> flatmap(Function<T, Stream<R>> proc, Stream<T> s)
    {
        return flatten(mapStream(proc, s));
    }
    
    public static <T> boolean noneMatch(Predicate<T> pred, Stream<T> s)
    {
        return isEmptyStream(filterStream(pred, s));
    }
    
    public static <T> T nthStream(int index, Stream<T> s)
    {
        if(isEmptyStream(s))
        {
            throw new RuntimeException("This index does not exist.");
        }
        
        return index == 0 ? head(s) : nthStream(index - 1, tail(s));
    }
    
    public static <T> void printStream(Stream<T> s)
    {
        if(isEmptyStream(s))
        {
            System.out.println("Done.\n");
            return;
        }
        
        System.out.println(head(s).toString());
        printStream(tail(s));        
    }
    
    public static <T> void printStreamLimit(int num, Stream<T> s)
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
    
    public static Stream<Integer> integersFrom(int n)
    {
        return consStream(n, () -> integersFrom(n + 1));
    }
    
    public static Stream<Integer> addStream(Stream<Integer> s1, Stream<Integer> s2)
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
        
        return consStream(head(s1) + head(s2), () -> addStream(tail(s1), tail(s2)));
    }
    
    public static Stream<Integer> scale(int c, Stream<Integer> s)
    {
        if(isEmptyStream(s))
        {
            return theEmptyStream();
        }
        
        return consStream(c * (Integer)head(s), () -> scale(c, tail(s)));
    }
    
    public static Stream<Integer> ones()
    {
        return consStream(1, () -> ones());
    }
    
    public static Stream<Integer> integers()
    {
        return consStream(1, () -> addStream(integers(), ones()));
    }
    
    public static Stream<Integer> fibs(int n1, int n2)
    {
        return consStream(n1, () -> fibs(n2, n1 + n2));
    }
    
    public static Stream<Integer> fibs()
    {
        return consStream(0, () -> consStream(1, () -> addStream(fibs(), tail(fibs()))));
    }
    
    public static Stream<Double> doubleOnes()
    {
        return consStream(1., () -> doubleOnes());
    }
    
    public static Stream<Double> doubles()
    {
        return consStream(1., () -> addDoubleStream(doubles(), doubleOnes()));
    }
    
    public static Stream<Double> doubles(double low, double high)
    {
        if(low > high)
        {
            return theEmptyStream();
        }
        
        return consStream(low, () -> doubles(low + 1., high));
    }
    
    public static Stream<Double> doublesFrom(double d)
    {
        return consStream(d, () -> doublesFrom(d + 1.));
    }
    
    public static Stream<Double> addDoubleStream(Stream<Double> s1, Stream<Double> s2)
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
        
        return consStream(head(s1) + head(s2), () -> addDoubleStream(tail(s1), tail(s2)));
    }
    
    public static Stream<Double> doubleScale(double c, Stream<Double> s)
    {
        if(isEmptyStream(s))
        {
            return theEmptyStream();
        }
        
        return consStream(c * (Double)head(s), () -> doubleScale(c, tail(s)));
    }
    
    //Auxiliary methods
    @SafeVarargs
    static <T> Stream<T> listToStream(T...elements)
    {
        return listToStream(new ArrayList<>(asList(elements)));
    }
    
    private static <T> Stream<T> listToStream(List<T> list)
    {
        if(list.isEmpty())
        {
            return theEmptyStream();
        }
        
        T head = list.remove(0);
        return consStream(head, () -> listToStream(list));
    }
    
    static <T> List<T> collectStream(Stream<T> s)
    {
        List<T> result = new ArrayList<>();
        
        if(s.isEmpty())
        {
            return result;
        }
        
        result.add(s.head());
        result.addAll(collectStream(s.tail()));
        
        return result;
    }
    
    public static <T> List<T> collectStreamLimit(int num, Stream<T> s)
    {
        List<T> result = new ArrayList<>();
        if(isEmptyStream(s))
        {
            return result;
        }
        
        collectStreamLimit(num, s, result);
        return result;        
    }
    
    private static <T> void collectStreamLimit(int num, Stream<T> s, List<T> result)
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

class Stream<T>
{
    private T head;
    private Supplier<Stream<T>> tail;

    public Stream(T head, Supplier<Stream<T>> tail)
    {
        this.head = head;
        this.tail = tail;
    }
    
    public T head()
    {
        return head;
    }

    public Stream<T> tail()
    {
        return tail.get();
    }

    public boolean isEmpty()
    {
        return null == head && null == tail;
    }
    
//    @Override
//    public String toString()
//    {
//        return String.join(", ", head.toString(), tail.toString());
//    }
}
