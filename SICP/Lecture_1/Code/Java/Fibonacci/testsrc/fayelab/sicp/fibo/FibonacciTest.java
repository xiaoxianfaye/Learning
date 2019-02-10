package fayelab.sicp.fibo;

import junit.framework.TestCase;

import static java.util.Arrays.asList;

import static fayelab.sicp.fibo.Fibonacci.*; 

public class FibonacciTest extends TestCase
{
    public void test_recursiveFibo()
    {
        assertEquals(asList(0, 1, 3, 5, 8), 
                     asList(recursiveFibo(0), recursiveFibo(1), recursiveFibo(4), recursiveFibo(5), recursiveFibo(6)));
    }
    
    public void test_iterationFibo()
    {
        assertEquals(asList(0, 1, 3, 5, 8), 
                     asList(iterationFibo(0), iterationFibo(1), iterationFibo(4), iterationFibo(5), iterationFibo(6)));
    }
    
    public void test_iterationButTailRecusiveFibo()
    {
        assertEquals(asList(0, 1, 3, 5, 8), 
                     asList(tailRecusiveFibo(0), tailRecusiveFibo(1), tailRecusiveFibo(4),
                            tailRecusiveFibo(5), tailRecusiveFibo(6)));
    }
}