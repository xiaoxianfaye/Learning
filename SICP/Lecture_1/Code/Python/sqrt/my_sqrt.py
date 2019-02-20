TOLERANCE = 0.001

def sqrt(x):
    return try_once(1.0, x)

def try_once(guess, x):
    if good_enough(guess, x):
        return guess
    else:
        return try_once(improve(guess, x), x)

def good_enough(guess, x):
    return abs(square(guess) - x) < TOLERANCE

def improve(guess, x):
    return average(guess, x / guess)

def square(x):
    return x * x

def average(x, y):
    return (x + y) / 2.0