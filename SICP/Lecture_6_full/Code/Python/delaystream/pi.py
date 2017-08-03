import math, random, sys

from stream import *

sys.setrecursionlimit(100000)

def random_stream():
    return cons_stream(randnum(), lambda : random_stream())

def randnum():
    return random.randint(1, 100000)

def cesaro_stream(randomS):
    return map_successive_pairs(lambda r1, r2: gcd(r1, r2) == 1, randomS)

def map_successive_pairs(func, s):
    return cons_stream(func(head(s), head(tail(s))), lambda : map_successive_pairs(func, tail(tail(s))))

def monte_carlo_stream(s, total, passed):
    passed = passed + 1 if head(s) else passed
    total = total + 1

    return cons_stream(1.0 * passed / total, lambda : monte_carlo_stream(tail(s), total, passed))

def pi_stream():
    return map_stream(lambda p: 0. if floatEquals(p, 0.) else math.sqrt(6. / p),
                      monte_carlo_stream(cesaro_stream(random_stream()), 0, 0))

def estimate(tolerance):
    return stream_limit(pi_stream(), tolerance)

def stream_limit(s, tolerance):
    return _stream_limit(head(s), tail(s), tolerance)

def _stream_limit(prev, s, tolerance):
    next = head(s)
    if(abs(next - prev) < tolerance):
        return next

    return _stream_limit(next, tail(s), tolerance)

def gcd(x, y):
    return x if y == 0 else gcd(y, x % y)

def floatEquals(f1, f2):
    return abs(f1 - f2) < 0.000000000000001

def main():
    print estimate(0.001)

if __name__ == '__main__':
    main()
