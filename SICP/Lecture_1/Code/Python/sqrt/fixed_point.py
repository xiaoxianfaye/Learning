TOLERANCE = 0.001

def sqrt(x):
    func = lambda y: lambda x: (y + x / y) / 2.0
    sqrt_func = (fixed_point(func))(1.0)
    return sqrt_func(x)

def cube_root(x):
    func = lambda y: lambda x: (x / (y * y) + 2 * y) / 3.0
    cube_root_func = (fixed_point(func))(1.0)
    return cube_root_func(x)

def fixed_point(func):
    return lambda guess: lambda x: try_once(guess, func, x)

def try_once(guess, func, x):
    next_guess = (func(guess))(x);
    if good_enough(next_guess, guess):
        return next_guess
    else:
        return try_once(next_guess, func, x)

def good_enough(next_guess, guess):
    return abs(next_guess - guess) < TOLERANCE

def average(x, y):
    return (x + y) / 2.0