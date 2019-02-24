package fayelab.sicp.hoproc.sum;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests
{
    public static Test suite()
    {
        TestSuite suite = new TestSuite(AllTests.class.getName());
        suite.addTestSuite(fayelab.sicp.hoproc.sum.nousinghoproc.SumTest.class);
        suite.addTestSuite(fayelab.sicp.hoproc.sum.usinghoproc.SumTest.class);
        return suite;
    }
}
