import unittest

from compound_data.rational_number.rational_number_without_gcd import *

class TestRationalNumberWithoutGcd(unittest.TestCase):
    def test_add(self):
        result = add(make_rational_number(1, 2), make_rational_number(1, 4))
        self.assertEqual(6, numer(result))
        self.assertEqual(8, denom(result))

    def test_mul(self):
        result = mul(make_rational_number(1, 2), make_rational_number(2, 3))
        self.assertEqual(2, numer(result))
        self.assertEqual(6, denom(result))