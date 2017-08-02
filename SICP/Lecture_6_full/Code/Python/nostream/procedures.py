## Procedures without stream
def sum_odds_square(obj):
    if isinstance(obj, tuple):
        return sum_odds_square(obj[0]) + sum_odds_square(obj[1])

    return square(obj) if isodd(obj) else 0

def odd_fibs(n):
    result = []
    
    for i in range(0, n + 1):
        if isodd(fib(i)):
            result.append(i)
    
    return result

def prime_sum_pairs(n):
    result = []

    for i in range(2, n + 1):
        for j in range(1, i):
            if isprime(i + j):
                result.append((i, j))

    return result

def triples(n):
    result = []

    for i in range(3, n + 1):
        for j in range(2, i):
            for k in range(1, j):
                result.append((i, j, k))

    return result

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
    for i in range(2, n):
        if n % i == 0:
            return False
    return True

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
