package fayelab.sicp.stream.nostream;

import java.util.Optional;

import junit.framework.TestCase;

import static java.util.Arrays.asList;
import static fayelab.sicp.stream.nostream.BtQueens.*;

public class BtQueensTest extends TestCase
{
    public void test_solveOne()
    {
        assertEquals(asList(asList(1, 2), asList(2, 4), asList(3, 1), asList(4, 3)), solveOne(4).get());
        
        assertEquals(Optional.empty(), solveOne(3));
        
        assertEquals(asList(asList(1, 1), asList(2, 5), asList(3, 8), asList(4, 6),
                            asList(5, 3), asList(6, 7), asList(7, 2), asList(8, 4)),
                     solveOne(8).get());
    }
    
    public void test_solveAll()
    {
        assertEquals(asList(asList(asList(1, 2), asList(2, 4), asList(3, 1), asList(4, 3)),
                            asList(asList(1, 3), asList(2, 1), asList(3, 4), asList(4, 2))), solveAll(4).get());
        
        assertEquals(Optional.empty(), solveAll(3));
        
        assertEquals(92, solveAll(8).get().size());
    }
}
