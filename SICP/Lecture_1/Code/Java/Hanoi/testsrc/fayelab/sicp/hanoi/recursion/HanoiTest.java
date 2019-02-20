package fayelab.sicp.hanoi.recursion;

import static fayelab.sicp.hanoi.recursion.Hanoi.*;
import static java.util.Arrays.asList;

import junit.framework.TestCase;

public class HanoiTest extends TestCase
{
    public void test_genMovingSeq()
    {
        assertEquals(asList(asList("1", "F", "T"), asList("2", "F", "S"), asList("1", "T", "S"),
                            asList("3", "F", "T"), asList("1", "S", "F"), asList("2", "S", "T"),
                            asList("1", "F", "T")),
                     genMovingSeq(3, "F", "T", "S"));
    }

    public void test_formatMovingSeq()
    {
        assertEquals(asList("1: F -> T", "2: F -> S"),
                     formatMovingSeq(asList(asList("1", "F", "T"), asList("2", "F", "S"))));
    }
}
