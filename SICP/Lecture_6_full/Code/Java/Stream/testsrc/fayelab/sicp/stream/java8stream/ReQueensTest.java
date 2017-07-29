package fayelab.sicp.stream.java8stream;

import java.util.Optional;

import junit.framework.TestCase;

import static java.util.Arrays.asList;
import static fayelab.sicp.stream.java8stream.ReQueens.*;

public class ReQueensTest extends TestCase
{
    public void test_solveAll()
    {
        assertEquals(asList(asList(asList(1, 2), asList(2, 4), asList(3, 1), asList(4, 3)),
                            asList(asList(1, 3), asList(2, 1), asList(3, 4), asList(4, 2))), solveAll(4).get());
        
        assertEquals(Optional.empty(), solveAll(3));
        
        assertEquals(92, solveAll(8).get().size());
    }
    
    public void test_solveAll2()
    {
        assertEquals(asList(asList(asList(1, 3), asList(2, 1), asList(3, 4), asList(4, 2)),
                            asList(asList(1, 2), asList(2, 4), asList(3, 1), asList(4, 3))), solveAll2(4).get());
        
        assertEquals(Optional.empty(), solveAll2(3));
        
        assertEquals(92, solveAll2(8).get().size());
    }
}
