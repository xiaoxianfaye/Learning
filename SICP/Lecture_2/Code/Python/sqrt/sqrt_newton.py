TOLERANCE = 0.00001
DELTA = 0.00001

def sqrt(x):
    return newton(lambda y: x - y * y, 1.0)

def cube_root(x):
    return newton(lambda y: x - y * y * y, 1.0)

def newton(func, guess):
    return fixed_point(lambda y: y - func(y) / (derivate(func))(y), guess)

def derivate(func):
    return lambda y: (func(y + DELTA) - func(y)) / DELTA

def fixed_point(func, guess):
    return _fixed_point(func, guess, func(guess))

def _fixed_point(func, old_guess, new_guess):
    if close_enough(old_guess, new_guess):
        return new_guess
    else:
        return _fixed_point(func, new_guess, func(new_guess))

def close_enough(old_guess, new_guess):
    return abs(new_guess - old_guess) < TOLERANCE