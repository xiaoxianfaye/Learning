package fayelab.sicp.compounddata.rationalnumber;

import static fayelab.sicp.compounddata.rationalnumber.RationalNumberWithGcd2.*;

import java.util.List;

import junit.framework.TestCase;

public class RationalNumberWithGcd2Test extends TestCase
{
    public void test_gcd()
    {
        assertEquals(2, gcd(6, 8));
    }
    
    public void test_add()
    {
        List<Integer> result = add(makeRationalNumber(1, 2), makeRationalNumber(1, 4));
        assertEquals(3, numer(result));
        assertEquals(4, denom(result));
    }

    public void test_mul()
    {
        List<Integer> result = mul(makeRationalNumber(1, 2), makeRationalNumber(2, 3));
        assertEquals(1, numer(result));
        assertEquals(3, denom(result));
    }
}