package fayelab.sicp.compounddata;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests
{
    public static Test suite()
    {
        TestSuite suite = new TestSuite(AllTests.class.getName());
        suite.addTest(fayelab.sicp.compounddata.rationalnumber.AllTests.suite());
        suite.addTestSuite(fayelab.sicp.compounddata.segment.SegmentTest.class);
        suite.addTestSuite(fayelab.sicp.compounddata.pair.PairTest.class);
        suite.addTestSuite(fayelab.sicp.compounddata.vec.VecTest.class);        
        return suite;
    }
}