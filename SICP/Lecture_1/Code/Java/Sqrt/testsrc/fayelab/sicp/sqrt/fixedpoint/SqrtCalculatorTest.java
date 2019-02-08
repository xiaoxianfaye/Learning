package fayelab.sicp.sqrt.fixedpoint;

import junit.framework.TestCase;

import static fayelab.sicp.sqrt.fixedpoint.SqrtCalculator.*;

public class SqrtCalculatorTest extends TestCase
{
    public void test_sqrt()
    {
        assertEquals(2.0, sqrt(4.0), TOLERANCE);
        assertEquals(3.0, sqrt(9.0), TOLERANCE);
    }
    
    public void test_cuberoot()
    {
        assertEquals(2.0, cubeRoot(8.0), TOLERANCE);
        assertEquals(3.0, cubeRoot(27.0), TOLERANCE);
    }
}