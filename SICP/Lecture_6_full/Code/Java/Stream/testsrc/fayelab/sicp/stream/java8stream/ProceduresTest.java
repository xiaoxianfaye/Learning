package fayelab.sicp.stream.java8stream;

import static fayelab.sicp.stream.java8stream.Procedures.*;
import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;

import junit.framework.TestCase;

public class ProceduresTest extends TestCase
{
    public void test_enumTree()
    {
        assertEquals(asList(1, 2, 7, 19, 12, 14), 
                     enumTree(biTuple(biTuple(1, biTuple(2, 7)), 
                                      biTuple(19, biTuple(12, 14))))
                     .collect(toList()));
        
        assertEquals(asList(1, 2, 3, 7, 19, 12, 13, 14), 
                     enumTree(biTuple(biTuple(1, biTuple(biTuple(2, 3), 7)), 
                                      biTuple(19, biTuple(12, biTuple(13, 14)))))
                     .collect(toList()));
    }
    
    public void test_enumInterval()
    {
        assertEquals(asList(2, 3, 4, 5), enumInterval(2, 5).collect(toList()));
    }
    
    public void test_sumOddsSquare()
    {
        BiTuple<?, ?> biTree = biTuple(biTuple(1, biTuple(2, 7)), biTuple(19, biTuple(12, 14)));
        assertEquals(411, sumOddsSquare(biTree));
    }
    
    public void test_oddFibs()
    {
        assertEquals(asList(1, 2, 4, 5), oddFibs(6));
    }
    
    public void test_primeSumPairs()
    {
        assertEquals(asList(asList(2, 1), asList(3, 2), asList(4, 1), asList(4, 3), asList(5, 2)), 
                primeSumPairs(5));
    }
    
    public void test_triples()
    {
        assertEquals(asList(asList(3, 2, 1),
                            asList(4, 2, 1), asList(4, 3, 1), asList(4, 3, 2),
                            asList(5, 2, 1), asList(5, 3, 1), asList(5, 3, 2), 
                            asList(5, 4, 1), asList(5, 4, 2), asList(5, 4, 3)),
                     triples(5));
    }
}
