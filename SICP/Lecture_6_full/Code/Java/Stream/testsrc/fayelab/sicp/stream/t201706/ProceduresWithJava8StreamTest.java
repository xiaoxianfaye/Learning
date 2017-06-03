package fayelab.sicp.stream.t201706;

import static java.util.Arrays.asList;

import junit.framework.TestCase;

import static fayelab.sicp.stream.t201706.ProceduresWithJava8Stream.*;

public class ProceduresWithJava8StreamTest extends TestCase
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
}
