import unittest

import fixed_point

class TestFixedPoint(unittest.TestCase):
    def test_sqrt(self):
        self.assertAlmostEqual(2.0, fixed_point.sqrt(4.0), delta=fixed_point.TOLERANCE)
        self.assertAlmostEqual(3.0, fixed_point.sqrt(9.0), delta=fixed_point.TOLERANCE)

    def test_cube_root(self):
        self.assertAlmostEqual(2.0, fixed_point.cube_root(8.0), delta=fixed_point.TOLERANCE)
        self.assertAlmostEqual(3.0, fixed_point.cube_root(27.0), delta=fixed_point.TOLERANCE)