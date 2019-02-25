TOLERANCE = 0.00001

def sqrt(x):
    return fixed_point(lambda y: average(y, x / y), 1.0)

def cube_root(x):
    return fixed_point(lambda y: (x / (y * y) + 2 * y) / 3.0, 1.0)

def fixed_point(func, guess):
    return _fixed_point(func, guess, func(guess))

def _fixed_point(func, old_guess, new_guess):
    if close_enough(old_guess, new_guess):
        return new_guess
    else:
        return _fixed_point(func, new_guess, func(new_guess))

def close_enough(old_guess, new_guess):
    return abs(new_guess - old_guess) < TOLERANCE

def average(x, y):
    return (x + y) / 2.0

def sqrt2(x):
    func = average_damp(lambda y: x / y)
    sqrt_func = (fixed_point(func))(1.0)
    return sqrt_func(x)

def average_damp(func):
    return lambda y: average(y, func(y))