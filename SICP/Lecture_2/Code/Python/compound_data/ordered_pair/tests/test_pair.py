import unittest

from compound_data.ordered_pair.pair import *

class TestPair(unittest.TestCase):
    def test_pair(self):
        p = make_pair(1, 2)
        self.assertEqual(1, head(p))
        self.assertEqual(2, tail(p))