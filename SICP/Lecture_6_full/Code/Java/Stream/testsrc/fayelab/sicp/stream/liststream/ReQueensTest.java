package fayelab.sicp.stream.liststream;

import java.util.Optional;

import junit.framework.TestCase;

import static java.util.Arrays.asList;
import static fayelab.sicp.stream.liststream.ReQueens.*;

public class ReQueensTest extends TestCase
{
    public void test_solveAll()
    {
        assertEquals(asList(asList(asList(1, 2), asList(2, 4), asList(3, 1), asList(4, 3)),
                            asList(asList(1, 3), asList(2, 1), asList(3, 4), asList(4, 2))), solveAll(4).get());
        
        assertEquals(Optional.empty(), solveAll(3));
        
        assertEquals(92, solveAll(8).get().size());
    }
}
