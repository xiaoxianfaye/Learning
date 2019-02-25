package fayelab.sicp.hoproc.sqrt.newton;

import static fayelab.sicp.hoproc.sqrt.newton.FixedPoint.TOLERANCE;

import fayelab.sicp.hoproc.sqrt.newton.SqrtCalculator;
import junit.framework.TestCase;

public class SqrtCalculatorTest extends TestCase
{
    public void test_sqrt()
    {
        assertTrue(Math.abs(SqrtCalculator.sqrt(4) - 2) < TOLERANCE);
    }
    
    public void test_cuberoot()
    {
        assertTrue(Math.abs(SqrtCalculator.cubeRoot(8) - 2) < TOLERANCE);
    }
}
