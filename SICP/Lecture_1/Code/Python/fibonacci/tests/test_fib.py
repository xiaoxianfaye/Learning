import unittest

from fibonacci.fib import recursive_fibo, iteration_fibo, tail_recursive_fibo

class TestFib(unittest.TestCase):
    def test_recursive_fibo(self):
        self.assertEqual([0, 1, 3, 5, 8],
                         [recursive_fibo(0), recursive_fibo(1), recursive_fibo(4),
                          recursive_fibo(5), recursive_fibo(6)])

    def test_iteration_fibo(self):
        self.assertEqual([0, 1, 3, 5, 8],
                         [iteration_fibo(0), iteration_fibo(1), iteration_fibo(4),
                          iteration_fibo(5), iteration_fibo(6)])

    def test_tail_recursive_fibo(self):
        self.assertEqual([0, 1, 3, 5, 8],
                         [tail_recursive_fibo(0), tail_recursive_fibo(1), tail_recursive_fibo(4),
                          tail_recursive_fibo(5), tail_recursive_fibo(6)])