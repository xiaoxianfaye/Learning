package fayelab.sicp.hanoi.iterconst;

import static fayelab.sicp.hanoi.iterconst.Hanoi.*;
import static java.util.Arrays.asList;

import java.util.List;
import java.util.function.Function;

import junit.framework.TestCase;


public class HanoiTest extends TestCase
{
    public void test_power2()
    {
        assertEquals(1, power2(0));
        assertEquals(2, power2(1));
        assertEquals(4, power2(2));
        assertEquals(8, power2(3));
    }

    public void test_isOdd()
    {
        assertTrue(isOdd(1));
        assertFalse(isOdd(2));
    }

    public void test_genEvenOddMovers()
    {
        List<Function<Integer, List<String>>> evenOddMovers = genEvenOddMovers(3, "F", "T", "S");
        Function<Integer, List<String>> evenMover = evenOddMovers.get(0);
        assertEquals(asList("F", "S"), evenMover.apply(1));
        assertEquals(asList("S", "T"), evenMover.apply(2));
        assertEquals(asList("T", "F"), evenMover.apply(0));
        Function<Integer, List<String>> oddMover = evenOddMovers.get(1);
        assertEquals(asList("F", "T"), oddMover.apply(1));
        assertEquals(asList("T", "S"), oddMover.apply(2));
        assertEquals(asList("S", "F"), oddMover.apply(0));

        List<Function<Integer, List<String>>> evenOddMovers2 = genEvenOddMovers(4, "F", "T", "S");
        Function<Integer, List<String>> evenMover2 = evenOddMovers2.get(0);
        assertEquals(asList("F", "T"), evenMover2.apply(1));
        assertEquals(asList("T", "S"), evenMover2.apply(2));
        assertEquals(asList("S", "F"), evenMover2.apply(0));
        Function<Integer, List<String>> oddMover2 = evenOddMovers2.get(1);
        assertEquals(asList("F", "S"), oddMover2.apply(1));
        assertEquals(asList("S", "T"), oddMover2.apply(2));
        assertEquals(asList("T", "F"), oddMover2.apply(0));
    }

    public void test_calcDiskNoAndCircularStepNo()
    {
        assertEquals(asList(1, 1), calcDiskNoAndCircularStepNo(1));
        assertEquals(asList(2, 1), calcDiskNoAndCircularStepNo(2));
        assertEquals(asList(1, 2), calcDiskNoAndCircularStepNo(3));
        assertEquals(asList(3, 1), calcDiskNoAndCircularStepNo(4));
        assertEquals(asList(1, 0), calcDiskNoAndCircularStepNo(5));
        assertEquals(asList(2, 2), calcDiskNoAndCircularStepNo(6));
        assertEquals(asList(1, 1), calcDiskNoAndCircularStepNo(7));
    }

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
