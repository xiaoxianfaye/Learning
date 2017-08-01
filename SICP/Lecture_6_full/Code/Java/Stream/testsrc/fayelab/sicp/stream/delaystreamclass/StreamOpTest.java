package fayelab.sicp.stream.delaystreamclass;

import junit.framework.TestCase;

import static java.util.Arrays.asList;
import static fayelab.sicp.stream.delaystreamclass.StreamOp.*;

public class StreamOpTest extends TestCase
{
    public void test_stream_constructors_and_selectors()
    {
        assertEquals(asList(1, 2, 3), collectStream(consStream(1, () -> listToStream(2, 3))));
        assertEquals(asList(1), collectStream(consStream(1, () -> listToStream())));
        assertEquals(asList(), collectStream(theEmptyStream()));
    }
    
    public void test_mapStream()
    {
        assertEquals(asList(2, 4), collectStream(mapStream(x -> x * 2, listToStream(1, 2))));
        assertEquals(asList("1", "2"), collectStream(mapStream(x -> String.valueOf(x), listToStream(1, 2))));
        assertEquals(asList(), collectStream(mapStream(x -> x, listToStream())));
    }
    
    public void test_filterStream()
    {
        assertEquals(asList(2, 4), collectStream(filterStream(x -> x % 2 == 0, listToStream(1, 2, 3, 4))));
        assertEquals(asList(), collectStream(filterStream(x -> true, listToStream())));
    }
    
    public void test_accStream()
    {
        assertEquals(new Integer(10), accStream((x, y) -> x + y, 0, listToStream(1, 2, 3, 4)));
        assertEquals("123", accStream((x, y) -> String.join("", String.valueOf(x), String.valueOf(y)), "", listToStream(1, 2, 3)));
        assertEquals(new Integer(0), accStream((Integer x, Integer y) -> x + y, 0, listToStream()));
        assertEquals(new Integer(1), accStream((x, y) -> x + y, 0, listToStream(1)));
    }
    
    public void test_appendStream()
    {
        assertEquals(asList(1, 2, 3, 4), collectStream(appendStream(listToStream(1, 2), listToStream(3, 4))));
        assertEquals(asList(3, 4), collectStream(appendStream(listToStream(), listToStream(3, 4))));
        assertEquals(asList(1, 2), collectStream(appendStream(listToStream(1, 2), listToStream())));
        assertEquals(asList(), collectStream(appendStream(listToStream(), listToStream())));
    }
    
    public void test_enumTree()
    {
        assertEquals(asList(1, 2, 7, 19, 12, 14), 
                     collectStream(enumTree(biTuple(biTuple(1, biTuple(2, 7)), 
                                            biTuple(19, biTuple(12, 14))))));
        
        assertEquals(asList(1, 2, 3, 7, 19, 12, 13, 14), 
                     collectStream(enumTree(biTuple(biTuple(1, biTuple(biTuple(2, 3), 7)), 
                                            biTuple(19, biTuple(12, biTuple(13, 14)))))));
    }
    
    public void test_enumInterval()
    {
        assertEquals(asList(1, 2, 3, 4), collectStream(enumInterval(1, 4)));
        assertEquals(asList(), collectStream(enumInterval(3, 1)));
    }
    
    public void test_flatten()
    {
        assertEquals(asList(1, 2, 3, 4, 5), collectStream(flatten(listToStream(listToStream(1, 2), 
                                                                               listToStream(3, 4), 
                                                                               listToStream(5)))));
        assertEquals(asList(1, 2), collectStream(flatten(listToStream(listToStream(1, 2)))));
        assertEquals(asList(), collectStream(flatten(listToStream())));
    }
    
    public void test_flatmap()
    {
        assertEquals(asList(1, 2, 3, 2, 4, 6), collectStream(flatmap(x -> listToStream(x, x * 2, x * 3), listToStream(1, 2))));
    }
    
    public void test_noneMatch()
    {
        assertTrue(noneMatch(x -> x % 2 == 0, listToStream(1, 3, 5)));
    }
    
    public void test_nthStream()
    {
        Stream<Integer> s = listToStream(0, 1);
        assertEquals(new Integer(0), nthStream(0, s));
        assertEquals(new Integer(1), nthStream(1, s));
        
        try
        {
            nthStream(2, s);
            assertTrue(false);
        }
        catch(RuntimeException e)
        {
            assertEquals("This index does not exist.", e.getMessage());
        }
    }
    
//    public void test_printStream()
//    {
//        printStream(listToStream(0, 1));
//    }
    
    public void test_integersFrom()
    {
        Stream<Integer> s = integersFrom(2);
        assertEquals(new Integer(2), nthStream(0, s));
        assertEquals(new Integer(3), nthStream(1, s));
        
        Stream<Integer> s2 = integersFrom(0);
        assertEquals(asList(0, 1, 2), collectStreamLimit(3, s2));
    }
    
    public void test_addStream()
    {
        assertEquals(asList(4, 6), collectStream(addStream(enumInterval(1, 2), enumInterval(3, 4))));
        assertEquals(asList(), collectStream(addStream(theEmptyStream(), theEmptyStream())));
        assertEquals(asList(1, 2), collectStream(addStream(enumInterval(1, 2), theEmptyStream())));
        assertEquals(asList(3, 4), collectStream(addStream(theEmptyStream(), enumInterval(3, 4))));
        assertEquals(asList(4, 4), collectStream(addStream(enumInterval(1, 1), enumInterval(3, 4))));
        assertEquals(asList(4, 2), collectStream(addStream(enumInterval(1, 2), enumInterval(3, 3))));
        
        assertEquals(asList(3, 5, 7, 9), collectStreamLimit(4, addStream(integersFrom(1), integersFrom(2))));
    }
    
    public void test_scale()
    {
        assertEquals(asList(2, 4), collectStream(scale(2, enumInterval(1, 2))));
        assertEquals(asList(), collectStream(scale(2, theEmptyStream())));
        
        assertEquals(asList(2, 4, 6, 8), collectStreamLimit(4, scale(2, integersFrom(1))));
    }
    
    public void test_ones()
    {
        assertEquals(asList(1, 1, 1, 1), collectStreamLimit(4, ones())); 
    }
    
    public void test_integers()
    {
        assertEquals(asList(1, 2, 3, 4), collectStreamLimit(4, integers())); 
    }
    
    public void test_fibs()
    {
        assertEquals(asList(0, 1, 1, 2, 3, 5), collectStreamLimit(6, fibs(0, 1)));
        assertEquals(asList(0, 1, 1, 2, 3, 5), collectStreamLimit(6, fibs()));
    }
    
    public void test_doubleOnes()
    {
        assertEquals(asList(1., 1., 1., 1.), collectStreamLimit(4, doubleOnes())); 
    }
    
    public void test_doubles()
    {
        assertEquals(asList(1., 2., 3., 4.), collectStreamLimit(4, doubles())); 
        assertEquals(asList(1., 2.), collectStreamLimit(2, doubles(1., 2.))); 
        assertEquals(asList(3., 4.), collectStreamLimit(2, doubles(3., 5.)));
    }
    
    public void test_doublesFrom()
    {
        Stream<Double> s = doublesFrom(2.);
        assertEquals(new Double(2.), nthStream(0, s));
        assertEquals(new Double(3.), nthStream(1, s));
        
        Stream<Double> s2 = doublesFrom(0.);
        assertEquals(asList(0., 1., 2.), collectStreamLimit(3, s2));
    }
    
    public void test_addDoubleStream()
    {
        assertEquals(asList(4., 6.), collectStream(addDoubleStream(doubles(1., 2.), doubles(3., 4.))));
        assertEquals(asList(), collectStream(addDoubleStream(theEmptyStream(), theEmptyStream())));
        assertEquals(asList(1., 2.), collectStream(addDoubleStream(doubles(1., 2.), theEmptyStream())));
        assertEquals(asList(3., 4.), collectStream(addDoubleStream(theEmptyStream(), doubles(3., 4.))));
        assertEquals(asList(4., 4.), collectStream(addDoubleStream(doubles(1., 1.), doubles(3., 4.))));
        assertEquals(asList(4., 2.), collectStream(addDoubleStream(doubles(1., 2.), doubles(3., 3.))));
        
        assertEquals(asList(3., 5., 7., 9.), collectStreamLimit(4, addDoubleStream(doublesFrom(1.), doublesFrom(2.))));
    }
    
    public void test_doubleScale()
    {
        assertEquals(asList(2, 4), collectStream(scale(2, enumInterval(1, 2))));
        assertEquals(asList(), collectStream(scale(2, theEmptyStream())));
        
        assertEquals(asList(2, 4, 6, 8), collectStreamLimit(4, scale(2, integersFrom(1))));
    }
}
