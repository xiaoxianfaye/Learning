import unittest

from hanoi_tower.hanoi_iterconst import *

class TestHanoiIterconst(unittest.TestCase):
    def test_power2(self):
        self.assertEqual(1, power2(0))
        self.assertEqual(2, power2(1))
        self.assertEqual(4, power2(2))
        self.assertEqual(8, power2(3))

    def is_odd(self):
        self.assertTrue(is_odd(1))
        self.assertFalse(is_odd(2))

    def test_gen_even_odd_movers(self):
        (even_mover, odd_mover) = gen_even_odd_movers(3, 'F', 'T', 'S')
        self.assertEqual(['F', 'S'], even_mover(1))
        self.assertEqual(['S', 'T'], even_mover(2))
        self.assertEqual(['T', 'F'], even_mover(0))
        self.assertEqual(['F', 'T'], odd_mover(1))
        self.assertEqual(['T', 'S'], odd_mover(2))
        self.assertEqual(['S', 'F'], odd_mover(0))
        (even_mover_2, odd_mover_2) = gen_even_odd_movers(4, 'F', 'T', 'S')
        self.assertEqual(['F', 'T'], even_mover_2(1))
        self.assertEqual(['T', 'S'], even_mover_2(2))
        self.assertEqual(['S', 'F'], even_mover_2(0))
        self.assertEqual(['F', 'S'], odd_mover_2(1))
        self.assertEqual(['S', 'T'], odd_mover_2(2))
        self.assertEqual(['T', 'F'], odd_mover_2(0))

    def test_calc_diskno_and_circular_stepno(self):
        self.assertEqual((1, 1), calc_diskno_and_circular_stepno(1))
        self.assertEqual((2, 1), calc_diskno_and_circular_stepno(2))
        self.assertEqual((1, 2), calc_diskno_and_circular_stepno(3))
        self.assertEqual((3, 1), calc_diskno_and_circular_stepno(4))
        self.assertEqual((1, 0), calc_diskno_and_circular_stepno(5))
        self.assertEqual((2, 2), calc_diskno_and_circular_stepno(6))
        self.assertEqual((1, 1), calc_diskno_and_circular_stepno(7))

    def test_gen_moving_seq(self):
        self.assertEqual([[1, 'F', 'T'], [2, 'F', 'S'], [1, 'T', 'S'],
                          [3, 'F', 'T'], [1, 'S', 'F'], [2, 'S', 'T'],
                          [1, 'F', 'T']],
                         gen_moving_seq(3, 'F', 'T', 'S'))

    def test_format_moving_seq(self):
        self.assertEqual(['1: F -> T', '2: F -> S'], format_moving_seq([[1, 'F', 'T'], [2, 'F', 'S']]))