TOLERANCE = 0.001

def sqrt(x):
    return try_once(1.0, x)

def try_once(guess, x):
    next_guess = average(guess, x / guess);
    if good_enough(next_guess, guess):
        return next_guess
    else:
        return try_once(next_guess, x)

def good_enough(next_guess, guess):
    return abs(next_guess - guess) < TOLERANCE

def average(x, y):
    return (x + y) / 2.0