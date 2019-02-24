import unittest

from sum.sum_using_hoproc import *

class TestSumUsingHoproc(unittest.TestCase):
    def test_sum_primitive(self):
        self.assertEqual(55, sum_primitive(1, 10))
        self.assertEqual(55, sum_primitive_r(1, 10))
        self.assertEqual(55, sum_primitive_tr(1, 10))

    def test_sum_square(self):
        self.assertEqual(30, sum_square(1, 4))
        self.assertEqual(30, sum_square_r(1, 4))
        self.assertEqual(30, sum_square_tr(1, 4))

    def test_sum_pi(self):
        TOLERANCE = 0.00001
        self.assertAlmostEqual(0.372005772006, sum_pi(1, 10), delta=TOLERANCE)
        self.assertAlmostEqual(0.372005772006, sum_pi_r(1, 10), delta=TOLERANCE)
        self.assertAlmostEqual(0.372005772006, sum_pi_tr(1, 10), delta=TOLERANCE)