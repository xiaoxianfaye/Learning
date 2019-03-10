import unittest

from compound_data.vector.vec import *

class TestVec(unittest.TestCase):
    def test_add(self):
        self.assertEqual([4, 6], add(make_vec(1, 2), make_vec(3, 4)))

    def test_scale(self):
        self.assertEqual([2, 4], scale(2, make_vec(1, 2)))