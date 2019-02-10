package fayelab.sicp.hanoi.recursive;

import junit.framework.TestCase;

import static java.util.Arrays.asList;
import static fayelab.sicp.hanoi.recursive.Hanoi.*;

public class HanoiTest extends TestCase
{
    public void test_genMovingSeq()
    {
        assertEquals(asList(asList("1", "F", "T"), asList("2", "F", "S"), asList("1", "T", "S"),
                            asList("3", "F", "T"), asList("1", "S", "F"), asList("2", "S", "T"),
                            asList("1", "F", "T")),
                     genMovingSeq(3, "F", "T", "S"));
    }
}
