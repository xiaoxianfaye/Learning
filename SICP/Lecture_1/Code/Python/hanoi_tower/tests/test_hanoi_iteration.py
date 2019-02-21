import unittest

from hanoi_tower.hanoi_iteration import *

class TestHanoiIteration(unittest.TestCase):
    def test_power2(self):
        self.assertEqual(1, power2(0))
        self.assertEqual(2, power2(1))
        self.assertEqual(4, power2(2))
        self.assertEqual(8, power2(3))

    def is_odd(self):
        self.assertTrue(is_odd(1))
        self.assertFalse(is_odd(2))

    def test_init_disk_movings(self):
        expected = [DiskMoving(1, 1, 2, 'F', 'T', 'S'),
                    DiskMoving(2, 2, 4, 'F', 'S', 'T'),
                    DiskMoving(3, 4, 8, 'F', 'T', 'S')]
        self.assertDiskMovings(expected, init_disk_movings(3, 'F', 'T', 'S'))

    def test_gen_moving_seq(self):
        self.assertEqual([[1, 'F', 'T'], [2, 'F', 'S'], [1, 'T', 'S'],
                          [3, 'F', 'T'], [1, 'S', 'F'], [2, 'S', 'T'],
                          [1, 'F', 'T']],
                         gen_moving_seq(3, 'F', 'T', 'S'))

    def test_format_moving_seq(self):
        self.assertEqual(['1: F -> T', '2: F -> S'], format_moving_seq([[1, 'F', 'T'], [2, 'F', 'S']]))

    def assertDiskMovings(self, expected, actual):
        self.assertEqual(len(expected), len(actual))
        for index in range(len(expected)):
            self.assertDiskMoving(expected[index], actual[index])

    def assertDiskMoving(self, expected, actual):
        self.assertEqual(expected.diskno, actual.diskno)
        self.assertEqual(expected.stepno, actual.stepno)
        self.assertEqual(expected.steps, actual.steps)
        self.assertEqual(expected.src, actual.src)
        self.assertEqual(expected.dest, actual.dest)
        self.assertEqual(expected.spare, actual.spare)