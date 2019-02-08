import unittest

import my_sqrt_2

class TestMySqrt2(unittest.TestCase):
    def test_sqrt(self):
        self.assertAlmostEqual(2.0, my_sqrt_2.sqrt(4.0), delta=my_sqrt_2.TOLERANCE)
        self.assertAlmostEqual(3.0, my_sqrt_2.sqrt(9.0), delta=my_sqrt_2.TOLERANCE)
