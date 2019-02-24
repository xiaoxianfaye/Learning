package fayelab.sicp.hoproc.sum.usinghoproc;

import static fayelab.sicp.hoproc.sum.usinghoproc.Sum.*;

import junit.framework.TestCase;

public class SumTest extends TestCase
{
    public void test_sumPrimitive()
    {
        doubleEquals(55, sum(1, 10));
        doubleEquals(55, sumPrimitive_r(1, 10));
        doubleEquals(55, sumPrimitive_tr(1, 10));
    }

    public void test_sumSquare()
    {
        doubleEquals(30, sumSquare(1, 4));
        doubleEquals(30, sumSquare_r(1, 4));
        doubleEquals(30, sumSquare_tr(1, 4));
    }

    public void test_sumPi()
    {
        doubleEquals(0.372005772005772, sumPi(1, 10));
        doubleEquals(0.372005772005772, sumPi_r(1, 10));
        doubleEquals(0.372005772005772, sumPi_tr(1, 10));
    }

    private void doubleEquals(double d1, double d2)
    {
        assertTrue(Math.abs(d1 - d2) < TOLERANCE);
    }
}