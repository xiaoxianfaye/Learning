import math, random

def estimate(count):
    return math.sqrt(6. / montecarlo(count, cesaro()))

def montecarlo(count, func):
    trueCount = len(filter(lambda b: b, map(lambda c: func(), range(1, count + 1))))
    return 1.0 * trueCount / count;

def cesaro():
    return lambda : gcd(randnum(), randnum()) == 1

def randnum():
    return random.randint(1, 100000)

def gcd(x, y):
    return x if y == 0 else gcd(y, x % y)

def profile(*counts):
    print map(lambda count: estimate(count), list(counts))
    
def main():
    print estimate(10000)
    profile(1000, 10000, 100000, 1000000, 10000000)

if __name__ == '__main__':
    main()
