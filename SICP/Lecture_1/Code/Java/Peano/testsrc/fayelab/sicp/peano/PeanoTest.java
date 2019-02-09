package fayelab.sicp.peano;

import junit.framework.TestCase;

import static fayelab.sicp.peano.Peano.*;

public class PeanoTest extends TestCase
{
    public void test_add_iteration()
    {
        assertEquals(3, iterationAdd(1, 2));
    }
    
    public void test_add_recursive()
    {
        assertEquals(3, recursiveAdd(1, 2));
    }
}