package fayelab.sicp.sqrt.first;

import junit.framework.TestCase;

import static fayelab.sicp.sqrt.first.SqrtCalculator.*;

public class SqrtCalculatorTest extends TestCase
{
    public void test_sqrt()
    {
        assertEquals(2.0, sqrt(4.0), TOLERANCE);
        assertEquals(3.0, sqrt(9.0), TOLERANCE);
    }
}