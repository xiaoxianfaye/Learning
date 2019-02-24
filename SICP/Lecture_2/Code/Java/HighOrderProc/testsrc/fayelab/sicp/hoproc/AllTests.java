package fayelab.sicp.hoproc;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests
{
    public static Test suite()
    {
        TestSuite suite = new TestSuite(AllTests.class.getName());
        suite.addTest(fayelab.sicp.hoproc.sum.AllTests.suite());
        return suite;
    }
}