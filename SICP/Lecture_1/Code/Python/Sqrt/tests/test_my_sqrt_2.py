import unittest

from sqrt.my_sqrt_2 import sqrt, TOLERANCE

class TestMySqrt2(unittest.TestCase):
    def test_sqrt(self):
        self.assertAlmostEqual(2.0, sqrt(4.0), delta=TOLERANCE)
        self.assertAlmostEqual(3.0, sqrt(9.0), delta=TOLERANCE)
