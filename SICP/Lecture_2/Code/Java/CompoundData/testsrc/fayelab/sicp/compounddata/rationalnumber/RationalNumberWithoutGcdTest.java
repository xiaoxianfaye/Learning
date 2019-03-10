package fayelab.sicp.compounddata.rationalnumber;

import static fayelab.sicp.compounddata.rationalnumber.RationalNumberWithoutGcd.*;

import java.util.List;

import junit.framework.TestCase;

public class RationalNumberWithoutGcdTest extends TestCase
{
    public void test_add()
    {
        List<Integer> result = add(makeRationalNumber(1, 2), makeRationalNumber(1, 4));
        assertEquals(6, numer(result));
        assertEquals(8, denom(result));
    }

    public void test_mul()
    {
        List<Integer> result = mul(makeRationalNumber(1, 2), makeRationalNumber(2, 3));
        assertEquals(2, numer(result));
        assertEquals(6, denom(result));
    }
}
