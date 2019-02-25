import unittest

from sqrt.sqrt_fixed_point import *

class TestSqrtFixedPoint(unittest.TestCase):
    def test_sqrt(self):
        self.assertAlmostEqual(2.0, sqrt(4.0), delta=TOLERANCE)
        self.assertAlmostEqual(3.0, sqrt(9.0), delta=TOLERANCE)
        self.assertAlmostEqual(2.0, sqrt2(4.0), delta=TOLERANCE)
        self.assertAlmostEqual(3.0, sqrt2(9.0), delta=TOLERANCE)

    def test_cube_root(self):
        self.assertAlmostEqual(2.0, cube_root(8.0), delta=TOLERANCE)
        self.assertAlmostEqual(3.0, cube_root(27.0), delta=TOLERANCE)