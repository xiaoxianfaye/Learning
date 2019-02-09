import unittest

import peano

class TestPeano(unittest.TestCase):
    def test_iteration_add(self):
        self.assertEqual(3, peano.iteration_add(1, 2))

    def test_recursive_add(self):
        self.assertEqual(3, peano.recursive_add(1, 2))