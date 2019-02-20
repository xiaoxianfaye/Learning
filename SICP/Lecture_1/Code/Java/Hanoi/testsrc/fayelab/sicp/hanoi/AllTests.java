package fayelab.sicp.hanoi;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests
{
    public static Test suite()
    {
        TestSuite suite = new TestSuite(AllTests.class.getName());
        suite.addTestSuite(fayelab.sicp.hanoi.iteration.HanoiTest.class);
        suite.addTestSuite(fayelab.sicp.hanoi.iterconst.HanoiTest.class);
        suite.addTestSuite(fayelab.sicp.hanoi.recursion.HanoiTest.class);
        return suite;
    }
}
