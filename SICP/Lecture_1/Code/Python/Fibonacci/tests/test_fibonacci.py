import unittest

import fibonacci

class TestFibonacci(unittest.TestCase):
    def test_recursive_fibo(self):
        self.assertEqual([0, 1, 3, 5, 8],
                         [fibonacci.recursive_fibo(0), fibonacci.recursive_fibo(1), fibonacci.recursive_fibo(4),
                          fibonacci.recursive_fibo(5), fibonacci.recursive_fibo(6)])

    def test_iteration_fibo(self):
        self.assertEqual([0, 1, 3, 5, 8],
                         [fibonacci.iteration_fibo(0), fibonacci.iteration_fibo(1), fibonacci.iteration_fibo(4),
                          fibonacci.iteration_fibo(5), fibonacci.iteration_fibo(6)])

    def test_tail_recursive_fibo(self):
        self.assertEqual([0, 1, 3, 5, 8],
                         [fibonacci.tail_recursive_fibo(0), fibonacci.tail_recursive_fibo(1),
                          fibonacci.tail_recursive_fibo(4), fibonacci.tail_recursive_fibo(5),
                          fibonacci.tail_recursive_fibo(6)])