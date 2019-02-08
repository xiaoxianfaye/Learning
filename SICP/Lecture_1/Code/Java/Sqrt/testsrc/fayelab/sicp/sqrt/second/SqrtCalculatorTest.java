package fayelab.sicp.sqrt.second;

import junit.framework.TestCase;

import static fayelab.sicp.sqrt.second.SqrtCalculator.*;

public class SqrtCalculatorTest extends TestCase
{
    public void test_sqrt()
    {
        assertEquals(2.0, sqrt(4.0), TOLERANCE);
        assertEquals(3.0, sqrt(9.0), TOLERANCE);
    }
}