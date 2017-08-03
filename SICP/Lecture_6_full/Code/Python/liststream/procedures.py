## Procedures with list stream
from stream import *

def sum_odds_square(biTree):
    return acc_stream(lambda x, y: x + y, 
                      0,
                      map_stream(lambda x: square(x), 
                                 filter_stream(lambda x: isodd(x), 
                                               enum_tree(biTree))))
                  

def odd_fibs(n):
    return acc_stream(lambda idx, acc: [idx] + acc, 
                      [], 
                      filter_stream(lambda idx: isodd(fib(idx)), 
                                    enum_interval(0, n)))

def prime_sum_pairs(n):
    return filter_stream(lambda pair: isprime(pair[0] + pair[1]),
                         flatmap(lambda i: map_stream(lambda j: (i, j), enum_interval(1, i - 1)),
                                 enum_interval(2, n)))


def triples(n):
    return flatmap(lambda i: flatmap(lambda j: map_stream(lambda k: (i, j, k), 
                                                          enum_interval(1, j - 1)), 
                                     enum_interval(2, i - 1)),
                   enum_interval(3, n))

## Auxiliary functions
def isodd(n):
    return n % 2 != 0

def square(n):
    return n * n

def fib(n):
    if n == 0:
        return 0
    if n == 1:
        return 1

    return fib(n - 1) + fib(n - 2)

def isprime(n):
    return all(map(lambda x: n % x != 0, range(2, n)))

## Tests
def test_sum_odds_square():
    assert 411 == sum_odds_square(((1, (2, 7)), (19, (12, 14))))
    print 'test_sum_odds_square ok'

def test_odd_fibs():
    assert [1, 2, 4, 5] == odd_fibs(6)
    print 'test_odd_fibs ok' 

def test_prime_sum_pairs():
    assert [(2, 1), (3, 2), (4, 1), (4, 3), (5, 2)] == prime_sum_pairs(5)
    print 'test_prime_sum_pairs ok'

def test_triples():
    assert [(3, 2, 1),
            (4, 2, 1), (4, 3, 1), (4, 3, 2),
            (5, 2, 1), (5, 3, 1), (5, 3, 2), (5, 4, 1), (5, 4, 2), (5, 4, 3)] == triples(5)
    print 'test_triples ok'

def test():
    test_sum_odds_square()
    test_odd_fibs()
    test_prime_sum_pairs()
    test_triples()

    print 'test ok'

if __name__ == '__main__':
    test()