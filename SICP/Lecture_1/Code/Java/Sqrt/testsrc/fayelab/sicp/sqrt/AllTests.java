package fayelab.sicp.sqrt;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests
{
    public static Test suite()
    {
        TestSuite suite = new TestSuite(AllTests.class.getName());
        suite.addTestSuite(fayelab.sicp.sqrt.first.SqrtCalculatorTest.class);
        suite.addTestSuite(fayelab.sicp.sqrt.second.SqrtCalculatorTest.class);
        suite.addTestSuite(fayelab.sicp.sqrt.fixedpoint.SqrtCalculatorTest.class);
        return suite;
    }
}