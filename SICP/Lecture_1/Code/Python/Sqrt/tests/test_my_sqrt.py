import unittest

import my_sqrt

class TestMySqrt(unittest.TestCase):
    def test_sqrt(self):
        self.assertAlmostEqual(2.0, my_sqrt.sqrt(4.0), delta=my_sqrt.TOLERANCE)
        self.assertAlmostEqual(3.0, my_sqrt.sqrt(9.0), delta=my_sqrt.TOLERANCE)
