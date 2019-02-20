import unittest

from peano_add.peano import iteration_add, recursive_add

class TestPeano(unittest.TestCase):
    def test_iteration_add(self):
        self.assertEqual(3, iteration_add(1, 2))

    def test_recursive_add(self):
        self.assertEqual(3, recursive_add(1, 2))