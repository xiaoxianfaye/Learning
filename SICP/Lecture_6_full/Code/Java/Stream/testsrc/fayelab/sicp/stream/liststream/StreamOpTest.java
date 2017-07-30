package fayelab.sicp.stream.liststream;

import junit.framework.TestCase;

import static fayelab.sicp.stream.liststream.StreamOp.*;
import static java.util.Arrays.asList;

public class StreamOpTest extends TestCase
{
    public void test_consStream()
    {
        assertEquals(asList(1, 2, 3), consStream(1, asList(2, 3)));
        assertEquals(asList(1), consStream(1, asList()));
    }
    
    public void test_theEmptyStream()
    {
        assertEquals(asList(), theEmptyStream());
    }
    
    public void test_head()
    {
        assertEquals(new Integer(1), head(asList(1, 2, 3)));
    }
    
    public void test_tail()
    {
        assertEquals(asList(2, 3), tail(asList(1, 2, 3)));
    }
    
    public void test_isEmptyStream()
    {
        assertTrue(isEmptyStream(asList()));
    }
    
    public void test_mapStream()
    {
        assertEquals(asList(2, 4), mapStream(x -> 2 * x, asList(1, 2)));
        assertEquals(asList("1", "2"), mapStream(x -> String.valueOf(x), asList(1, 2)));
        assertEquals(asList(), mapStream(x -> x, asList()));
    }
    
    public void test_filterStream()
    {
        assertEquals(asList(2, 4), filterStream(x -> x % 2 == 0, asList(1, 2, 3, 4)));
        assertEquals(asList(), filterStream(x -> true, asList()));
    }
    
    public void test_accStream()
    {
        assertEquals(new Integer(10), accStream((x, y) -> x + y, 0, asList(1, 2, 3, 4)));
        assertEquals("123", accStream((x, y) -> String.join("", String.valueOf(x), String.valueOf(y)), "", asList(1, 2, 3)));
        assertEquals(new Integer(0), accStream((Integer x, Integer y) -> x + y, 0, asList()));
        assertEquals(new Integer(1), accStream((x, y) -> x + y, 0, asList(1)));
    }
    
    public void test_appendStream()
    {
        assertEquals(asList(1, 2, 3, 4), appendStream(asList(1, 2), asList(3, 4)));
        assertEquals(asList(3, 4), appendStream(asList(), asList(3, 4)));
        assertEquals(asList(1, 2), appendStream(asList(1, 2), asList()));
        assertEquals(asList(), appendStream(asList(), asList()));
    }
    
    public void test_enumTree()
    {
        assertEquals(asList(1, 2, 7, 19, 12, 14), 
                     enumTree(biTuple(biTuple(1, biTuple(2, 7)), 
                              biTuple(19, biTuple(12, 14)))));
        
        assertEquals(asList(1, 2, 3, 7, 19, 12, 13, 14), 
                     enumTree(biTuple(biTuple(1, biTuple(biTuple(2, 3), 7)), 
                              biTuple(19, biTuple(12, biTuple(13, 14))))));
    }
    
    public void test_enumInterval()
    {
        assertEquals(asList(1, 2, 3, 4), enumInterval(1, 4));
        assertEquals(asList(), enumInterval(3, 1));
    }
    
    public void test_flatten()
    {
        assertEquals(asList(1, 2, 3, 4, 5), flatten(asList(asList(1, 2), asList(3, 4), asList(5))));
        assertEquals(asList(1, 2), flatten(asList(asList(1, 2))));
        assertEquals(asList(), flatten(asList()));
    }
    
    public void test_flatmap()
    {
        assertEquals(asList(1, 2, 3, 2, 4, 6), flatmap(x -> asList(x, x * 2, x * 3), asList(1, 2)));
    }
    
    public void test_noneMatch()
    {
        assertTrue(noneMatch(x -> x % 2 == 0, asList(1, 3, 5)));
    }
}
