package fayelab.sicp.compounddata.rationalnumber;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests
{
    public static Test suite()
    {
        TestSuite suite = new TestSuite(AllTests.class.getName());
        suite.addTestSuite(fayelab.sicp.compounddata.rationalnumber.RationalNumberWithoutGcdTest.class);
        suite.addTestSuite(fayelab.sicp.compounddata.rationalnumber.RationalNumberWithGcdTest.class);
        suite.addTestSuite(fayelab.sicp.compounddata.rationalnumber.RationalNumberWithGcd2Test.class);
        suite.addTestSuite(fayelab.sicp.compounddata.rationalnumber.RationalNumberWithPairTest.class);
        return suite;
    }
}