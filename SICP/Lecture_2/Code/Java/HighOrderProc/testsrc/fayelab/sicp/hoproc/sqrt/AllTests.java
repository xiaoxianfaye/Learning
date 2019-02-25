package fayelab.sicp.hoproc.sqrt;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests
{
    public static Test suite()
    {
        TestSuite suite = new TestSuite(AllTests.class.getName());
        suite.addTestSuite(fayelab.sicp.hoproc.sqrt.fixedpoint.SqrtCalculatorTest.class);
        suite.addTestSuite(fayelab.sicp.hoproc.sqrt.newton.SqrtCalculatorTest.class);
        return suite;
    }
}