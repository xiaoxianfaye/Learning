package fayelab.sicp.stream.delaystream;

import junit.framework.TestCase;

import static java.util.Arrays.asList;

import java.util.List;

import static fayelab.sicp.stream.delaystream.StreamOp.*;
import static fayelab.sicp.stream.delaystream.Procedures.*;

public class ProceduresTest extends TestCase
{
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
    
    public void test_secondPrime()
    {
        assertEquals(10009, secondPrime());
    }
    
    public void test_noSevens()
    {
        List<Object> s = noSevens();
        assertEquals(new Integer(1), nthStream(0, s));
        assertEquals(new Integer(8), nthStream(6, s));
        assertEquals(new Integer(16), nthStream(13, s));
        
        assertEquals(asList(1, 2, 3, 4, 5, 6, 8), collectStreamLimit(7, noSevens()));
    }
    
    public void test_primes()
    {
        assertEquals(asList(2, 3, 5, 7, 11, 13, 17, 19, 23, 29),  collectStreamLimit(10, primes()));
    }
    
}
