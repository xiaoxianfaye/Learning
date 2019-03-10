package fayelab.sicp.compounddata.segment;

import static fayelab.sicp.compounddata.segment.Segment.*;
import static java.util.Arrays.asList;

import junit.framework.TestCase;

public class SegmentTest extends TestCase
{
    public void test_middlePoint()
    {
        assertEquals(asList(2, 1), middlePoint(makeSegment(makeVec(0, 0), makeVec(4, 2))));
    }

    public void test_length()
    {
        assertEquals(5, length(makeSegment(makeVec(0, 0), makeVec(4, 3))));
    }
}