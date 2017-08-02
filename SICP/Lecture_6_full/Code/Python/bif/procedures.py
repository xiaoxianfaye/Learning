## Procedures with built-in functions
def sum_odds_square(biTree):
    return reduce(lambda x, y: x + y, 
                  map(lambda x: square(x), filter(lambda x: isodd(x), enum_tree(biTree))), 
                  0)

def odd_fibs(n):
    return [idx for idx in enum_interval(0, n) if isodd(fib(idx))]

def prime_sum_pairs(n):
    return [(i, j) for i in enum_interval(2, n) for j in enum_interval(1, i - 1) if isprime(i + j)]

def triples(n):
    return [(i, j, k) for i in enum_interval(3, n) for j in enum_interval(2, i - 1) for k in enum_interval(1, j - 1)]

def enum_tree(obj):
    result = []

    if isinstance(obj, tuple):
        return enum_tree(obj[0]) + enum_tree(obj[1])

    result.append(obj)
    return result

def enum_interval(low, high):
    return range(low, high + 1)

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
def test_enum_tree():
    assert [1, 2, 7, 19, 12, 14] == enum_tree(((1, (2, 7)), (19, (12, 14))))
    assert [1, 2, 3, 7, 19, 12, 13, 14] == enum_tree(((1, ((2, 3), 7)), (19, (12, (13, 14)))))
    print 'test_enum_tree ok'

def test_enum_interval():
    assert [2, 3, 4, 5] == enum_interval(2, 5)
    assert [] == enum_interval(3, 1)
    print 'test_enumInterval ok'

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
    test_enum_tree()
    test_enum_interval()
    test_sum_odds_square()
    test_odd_fibs()
    test_prime_sum_pairs()
    test_triples()

    print 'test ok'

if __name__ == '__main__':
    test()
