import unittest

from compound_data.segment.seg import *

class TestSeg(unittest.TestCase):
    def test_middle_point(self):
        self.assertEqual([2, 1], middle_point(make_seg(make_vec(0, 0), make_vec(4, 2))))

    def test_seg_length(self):
        self.assertEqual(5, seg_length(make_seg(make_vec(0, 0), make_vec(4, 3))))