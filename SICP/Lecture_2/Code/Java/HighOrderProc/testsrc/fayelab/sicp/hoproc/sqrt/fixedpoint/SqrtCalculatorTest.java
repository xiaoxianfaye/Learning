package fayelab.sicp.hoproc.sqrt.fixedpoint;

import static fayelab.sicp.hoproc.sqrt.fixedpoint.SqrtCalculator.*;
import static fayelab.sicp.hoproc.sqrt.fixedpoint.FixedPoint.TOLERANCE;

import junit.framework.TestCase;

public class SqrtCalculatorTest extends TestCase
{
    public void test_sqrt()
    {
        assertTrue(Math.abs(sqrt(4) - 2) < TOLERANCE);
        assertTrue(Math.abs(sqrt2(4) - 2) < TOLERANCE);
    }
    
    public void test_cuberoot()
    {
        assertTrue(Math.abs(cubeRoot(8) - 2) < TOLERANCE);
    }
}