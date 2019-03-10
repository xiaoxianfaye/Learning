package fayelab.sicp.compounddata.vec;

import static fayelab.sicp.compounddata.vec.Vec.*;
import static java.util.Arrays.asList;

import junit.framework.TestCase;

public class VecTest extends TestCase
{
    public static void test_add()
    {
        assertEquals(asList(4, 6), add(makeVec(1, 2), makeVec(3, 4)));
    }

    public static void test_scale()
    {
        assertEquals(asList(2, 4), scale(2, makeVec(1, 2)));
    }
}