import unittest

from hanoi_tower.hanoi_recursion import gen_moving_seq, format_moving_seq

class TestHanoiRecursion(unittest.TestCase):
    def test_gen_moving_seq(self):
        self.assertEqual([[1, 'F', 'T'], [2, 'F', 'S'], [1, 'T', 'S'],
                          [3, 'F', 'T'], [1, 'S', 'F'], [2, 'S', 'T'],
                          [1, 'F', 'T']],
                         gen_moving_seq(3, 'F', 'T', 'S'))

    def test_format_moving_seq(self):
        self.assertEqual(['1: F -> T', '2: F -> S'], format_moving_seq([[1, 'F', 'T'], [2, 'F', 'S']]))