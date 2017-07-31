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
    
    public void test_integral()
    {
        assertEquals(asList(0., 1., 2., 3., 4.), collectStreamLimit(5, integral(doubleOnes(), 0., 1.)));
    }
    
    public void test_funmaps()
    {
        assertEquals(asList(0., 1., 2., 3., 4.), collectStreamLimit(5, funmaps(x -> x, 0., 1.)));
    }
    
    public void test_funmaps_integral()
    {
        List<Object> idFs = funmaps(x -> x, 1., 0.001);
        List<Object> s1 = integral(idFs, 0., 0.001);
        Double actual1 = (Double)nthStream(1001, s1);
        System.out.println(actual1);
        assertTrue(Math.abs(1.5 - actual1) < 0.1);
        
        List<Object> sqFs = funmaps(x -> x * x, 1., 0.001);
        List<Object> s2 = integral(sqFs, 0., 0.001);
        Double actual2 = (Double)nthStream(1001, s2);
        System.out.println(actual2);
        assertTrue(Math.abs(2.33 - actual2) < 0.1);
        
        List<Object> sqFs2 = funmaps(x -> x * x, 0., 0.001);
        List<Object> s3 = integral(sqFs2, 0., 0.001);
        Double actual3 = (Double)nthStream(1001, s3);
        System.out.println(actual3);
        assertTrue(Math.abs(0.33 - actual3) < 0.1);
    }
    
    public void test_integrator()
    {
        List<Object> s1 = integrator(x -> x, 1, 0.01);
        Double actual1 = (Double)nthStream(101, s1);
        System.out.println(actual1);
        assertTrue(Math.abs(1.5 - actual1) < 0.1);
        
        List<Object> s2 = integrator(x -> x * x, 1, 0.01);
        Double actual2 = (Double)nthStream(101, s2);
        System.out.println(actual2);
        assertTrue(Math.abs(2.33 - actual2) < 0.1);
        
        List<Object> s3 = integrator(x -> x * x, 0, 0.01);
        Double actual3 = (Double)nthStream(101, s3);
        System.out.println(actual3);
        assertTrue(Math.abs(0.33 - actual3) < 0.1);
    }
    
//    public void test_integralWithDelay()
//    {
//        List<Object> s = ys();
//        Double actual = (Double)nthStream(11, s);
//        System.out.println(actual);
//    }
    
    public void test_sqrt()
    {
        assertTrue(Math.abs(1.414 - sqrt(2, 0.001)) < 0.001);
        assertTrue(Math.abs(3. - sqrt(9, 0.001)) < 0.001);
    }
    
    public void test_pair()
    {
        assertEquals(asList(asList(1, 1),
                            asList(1, 2), asList(2, 2),
                            asList(1, 3), asList(2, 3), asList(3, 3)),
                     collectStreamLimit(6, pair(integersFrom(1))));
        
        printStreamLimit(10, pair(integersFrom(1)));
    }
    
    public void test_pair2()
    {
        assertEquals(asList(asList(1, 1),
                            asList(1, 2), asList(2, 2),
                            asList(1, 3), asList(2, 3), asList(3, 3),
                            asList(1, 4), asList(2, 4), asList(3, 4), asList(4, 4)),
                     collectStreamLimit(10, pair(integersFrom(1), integersFrom(1))));
    }
    
    public void test_allPairs()
    {
        assertEquals(asList(asList(1, 1),
                            asList(1, 2),
                            asList(2, 1),
                            asList(2, 2),
                            asList(1, 3), asList(2, 3),
                            asList(3, 1), asList(3, 2),
                            asList(3, 3),
                            asList(1, 4), asList(2, 4), asList(3, 4),
                            asList(4, 1), asList(4, 2), asList(4, 3),
                            asList(4, 4)), 
                     collectStreamLimit(16, allPairs(integersFrom(1), integersFrom(1))));
    }
    
    public void test_reAllPairs()
    {
        assertEquals(asList(asList(1, 1), asList(1, 2), asList(2, 1), asList(1, 3),
                            asList(2, 2), asList(1, 4), asList(3, 1), asList(1, 5)), 
                     collectStreamLimit(8, reAllPairs(integersFrom(1), integersFrom(1))));
    }
    
    public void test_upPairs()
    {
        assertEquals(asList(asList(1, 1), asList(1, 2), asList(2, 2), asList(1, 3), 
                            asList(2, 3), asList(1, 4), asList(3, 3), asList(1, 5)), 
                     collectStreamLimit(8, upPairs(integersFrom(1), integersFrom(1))));
    }
}
