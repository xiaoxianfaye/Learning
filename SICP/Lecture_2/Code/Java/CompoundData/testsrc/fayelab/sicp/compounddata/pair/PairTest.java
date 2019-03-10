package fayelab.sicp.compounddata.pair;

import static fayelab.sicp.compounddata.pair.Pair.*;

import java.util.function.Function;

import junit.framework.TestCase;

public class PairTest extends TestCase
{
    public void test_pair()
    {
        Function<Integer, Integer> pair = makePair(1, 2);
        assertEquals(new Integer(1), head(pair));
        assertEquals(new Integer(2), tail(pair));
    }
}