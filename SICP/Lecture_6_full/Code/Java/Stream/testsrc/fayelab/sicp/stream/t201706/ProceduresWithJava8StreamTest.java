package fayelab.sicp.stream.t201706;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;

import junit.framework.TestCase;

import static fayelab.sicp.stream.t201706.ProceduresWithJava8Stream.*;

public class ProceduresWithJava8StreamTest extends TestCase
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
}
