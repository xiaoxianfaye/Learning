package fayelab.sicp.hanoi.iteration;

import static fayelab.sicp.hanoi.iteration.Hanoi.*;
import static java.util.Arrays.asList;

import java.util.List;
import java.util.stream.IntStream;

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

    public void test_initDiskMovings()
    {
        List<DiskMoving> expected = asList(
                new DiskMoving(1, 1, 2, "F", "T", "S"),
                new DiskMoving(2, 2, 4, "F", "S", "T"),
                new DiskMoving(3, 4, 8, "F", "T", "S"));
        assertDiskMovings(expected, initDiskMovings(3, "F", "T", "S"));
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

    private void assertDiskMovings(List<DiskMoving> expected, List<DiskMoving> actual)
    {
        assertEquals(expected.size(), actual.size());
        IntStream.range(0, expected.size())
                 .forEach(i -> assertDiskMoving(expected.get(i), actual.get(i)));
    }

    private void assertDiskMoving(DiskMoving expected, DiskMoving actual)
    {
        assertEquals(expected.getDiskNo(), actual.getDiskNo());
        assertEquals(expected.getStepNo(), actual.getStepNo());
        assertEquals(expected.getSteps(), actual.getSteps());
        assertEquals(expected.getFrom(), actual.getFrom());
        assertEquals(expected.getTo(), actual.getTo());
        assertEquals(expected.getSpare(), actual.getSpare());
    }
}
