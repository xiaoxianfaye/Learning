import unittest

from compound_data.rational_number.rational_number_with_pair import *
from compound_data.ordered_pair.pair import *

class TestRationalNumberWithPair(unittest.TestCase):
    def test_gcd(self):
        self.assertEqual(2, gcd(6, 8))

    def test_add(self):
        result = add(make_rational_number(1, 2), make_rational_number(1, 4))
        self.assertEqual(3, numer(result))
        self.assertEqual(4, denom(result))

    def test_mul(self):
        result = mul(make_rational_number(1, 2), make_rational_number(2, 3))
        self.assertEqual(1, numer(result))
        self.assertEqual(3, denom(result))