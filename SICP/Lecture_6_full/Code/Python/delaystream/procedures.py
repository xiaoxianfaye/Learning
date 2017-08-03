## Procedures with delay stream
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
    return collect_stream(
              filter_stream(lambda pair: isprime(pair[0] + pair[1]),
                            flatmap(lambda i: map_stream(lambda j: (i, j), enum_interval(1, i - 1)),
                                    enum_interval(2, n))))

def triples(n):
    return collect_stream(
              flatmap(lambda i: flatmap(lambda j: map_stream(lambda k: (i, j, k), 
                                                             enum_interval(1, j - 1)), 
                                        enum_interval(2, i - 1)),
                      enum_interval(3, n)))

def second_prime():
    return head(tail(filter_stream(lambda x: isprime(x), enum_interval(10000, 1000000))))

def no_sevens():
    return filter_stream(lambda x: x % 7 != 0, integers_from(1))

def primes():
    return sieve(integers_from(2))

def sieve(s):
    return cons_stream(head(s), lambda : sieve(filter_stream(lambda x: x % head(s) != 0, tail(s))))

def integral(s, init, dt):
    return cons_stream(init, lambda : add_stream(scale_stream(dt, s), integral(s, init, dt)))

def funmaps(func, init, dt):
    return cons_stream(func(init), lambda : funmaps(func, init + dt, dt))

def integrator(func, init, dt):
    return integral(map_stream(func, integral(ones(), init, dt)), 0, dt)

def integral_with_delay(delayS, init, dt):
    return cons_stream(init, lambda : add_stream(scale_stream(dt, delayS()), integral(delayS(), init, dt)))

def ys():
    return integral_with_delay(lambda : dys(), 1, 0.1)

def dys():
    return map_stream(lambda x: x * x, ys())

def sqrt(n, tolerance):
    return stream_limit(sqrt_stream(n), tolerance)

def sqrt_stream(n):
    return cons_stream(1.0, lambda : map_stream(lambda x: (x + n / x) / 2., sqrt_stream(n)))

def stream_limit(s, tolerance):
    return _stream_limit(head(s), tail(s), tolerance)

def _stream_limit(prev, s, tolerance):
    next = head(s)
    if(abs(next - prev) < tolerance):
        return next

    return _stream_limit(next, tail(s), tolerance)

def pair(s):
    return flatmap(lambda j: map_stream(lambda i: (i, j), enum_interval(1, j)), integers_from(1))

def pair2(s, t):
    return _pair2(s, t, lambda ele: the_empty_stream())

def _pair2(s, t, func):
    return append_stream(func(head(t)), 
                         cons_stream((head(s), head(t)), 
                                     lambda : _pair2(tail(s), tail(t), acc_func(func, head(s)))))

def acc_func(func, headS):
    return lambda ele: append_stream(func(ele), cons_stream((headS, ele), lambda : the_empty_stream()))

def all_pairs(s, t):
    return _all_pairs(s, t, lambda eleS: the_empty_stream(), lambda eleT: the_empty_stream())

def _all_pairs(s, t, sFunc, tFunc):
    return append_stream(sFunc(head(t)),
                         append_stream(tFunc(head(s)),
                                       cons_stream((head(s), head(t)),
                                                   lambda: _all_pairs(tail(s), tail(t),
                                                                      acc_first_func(sFunc, head(s)),
                                                                      acc_second_func(tFunc, head(t))))))

def acc_first_func(sFunc, headS):
    return lambda eleT: append_stream(sFunc(eleT), cons_stream((headS, eleT), lambda : the_empty_stream()))

def acc_second_func(tFunc, headT):
    return lambda eleS: append_stream(tFunc(eleS), cons_stream((eleS, headT), lambda : the_empty_stream()))

def re_all_pairs(s, t):
    return cons_stream((head(s), head(t)), lambda : interleave(map_stream(lambda eleT: (head(s), eleT), tail(t)),
                                                               re_all_pairs(tail(s), t)))

def interleave(s1, s2):
    if is_empty_stream(s1):
        return s2

    return cons_stream(head(s1), lambda : interleave(s2, tail(s1)))

def up_pair(s, t):
    return cons_stream((head(s), head(t)), lambda : interleave(map_stream(lambda eleT: (head(s), eleT), tail(t)),
                                                               up_pair(tail(s), tail(t))))

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

def test_second_prime():
    assert 10009 == second_prime()

    print 'test_second_prime ok'

def test_no_sevens():
    s = no_sevens()
    assert 1 == nth_stream(0, s)
    assert 8 == nth_stream(6, s)
    assert 16 == nth_stream(13, s)
  
    assert [1, 2, 3, 4, 5, 6, 8] == collect_stream_limit(7, no_sevens())

    print 'test_no_sevens ok'

def test_primes():
    s = primes()
    assert [2, 3, 5, 7, 11, 13, 17, 19, 23, 29] == collect_stream_limit(10, primes())

    print 'test_primes ok'

def test_integral():
    assert [0, 1, 2, 3, 4] == collect_stream_limit(5, integral(ones(), 0, 1))

    print 'test_integral ok'

def test_funmaps():
    assert [0, 1, 2, 3, 4] == collect_stream_limit(5, funmaps(lambda x: x, 0, 1))

    print 'test_funmaps ok'

def test_funmaps_integral():
    idFs = funmaps(lambda x: x, 1, 0.01)
    s1 = integral(idFs, 0, 0.01)
    actual1 = nth_stream(101, s1)
    print actual1
    assert abs(1.5 - actual1) < 0.1

    sqFs = funmaps(lambda x: x * x, 1, 0.01)
    s2 = integral(sqFs, 0, 0.01)
    actual2 = nth_stream(101, s2)
    print actual2
    assert abs(2.33 - actual2) < 0.1

    sqFs2 = funmaps(lambda x: x * x, 0, 0.01)
    s3 = integral(sqFs2, 0, 0.01)
    actual3 = nth_stream(101, s3)
    print actual3
    assert abs(0.33 - actual3) < 0.1

    print 'test_funmaps_integral ok'

def test_integrator():
    s1 = integrator(lambda x: x, 1, 0.01)
    actual1 = nth_stream(101, s1)
    print actual1
    assert abs(1.5 - actual1) < 0.1

    s2 = integrator(lambda x: x * x, 1, 0.01)
    actual2 = nth_stream(101, s2)
    print actual2
    assert abs(2.33 - actual2) < 0.1

    s3 = integrator(lambda x: x * x, 0, 0.01)
    actual3 = nth_stream(101, s3)
    print actual3
    assert abs(0.33 - actual3) < 0.1

    print 'test_integrator ok'

def test_integral_with_delay():
    s = ys();
    print nth_stream(11, s);
    
    print 'test_integral_with_delay ok'

def test_sqrt():
    assert abs(1.414 - sqrt(2, 0.001) < 0.001)
    assert abs(3 - sqrt(9, 0.001) < 0.001)

    print 'test_sqrt ok'

def test_pair():
    [(1, 1), (1, 2), (2, 2), (1, 3), (2, 3), (3, 3)] == collect_stream_limit(6, pair(integers_from(1)))
    print_stream_limit(10, pair(integers_from(1)))

    print 'test_pair ok'

def test_pair2():
    [(1, 1), 
     (1, 2), (2, 2), 
     (1, 3), (2, 3), (3, 3),
     (1, 4), (2, 4), (3, 4), (4, 4)] == collect_stream_limit(10, pair2(integers_from(1), integers_from(1)))

    print 'test_pair2 ok'

def test_all_pairs():
    [(1, 1),
     (1, 2),
     (2, 1),
     (2, 2),
     (1, 3), (2, 3),
     (3, 1), (3, 2),
     (3, 3),
     (1, 4), (2, 4), (3, 4),
     (4, 1), (4, 2), (4, 3),
     (4, 4)] == collect_stream_limit(16, all_pairs(integers_from(1), integers_from(1)))

    print 'test_all_pairs ok'

def test_re_all_pairs():
    [(1, 1), (1, 2), (2, 1), (1, 3), 
     (2, 2), (1, 4), (3, 1), (1, 5)] == collect_stream_limit(8, re_all_pairs(integers_from(1), integers_from(1)))

    print 'test_re_all_pairs ok'

def test_up_pair():
    [(1, 1), (1, 2), (2, 2), (1, 3), 
     (2, 3), (1, 4), (3, 3), (1, 5)] == collect_stream_limit(8, up_pair(integers_from(1), integers_from(1)))  

def test():
    test_sum_odds_square()
    test_odd_fibs()
    test_prime_sum_pairs()
    test_triples()
    test_second_prime()
    test_no_sevens()
    test_primes()
    test_integral()
    test_funmaps()
    test_funmaps_integral()
    test_integrator()
    test_integral_with_delay()
    test_sqrt()
    test_pair()
    test_pair2()
    test_all_pairs()
    test_re_all_pairs()
    test_up_pair()

    print 'test ok'

if __name__ == '__main__':
    test()