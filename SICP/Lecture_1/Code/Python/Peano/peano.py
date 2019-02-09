def iteration_add(x, y):
    if x == 0:
        return y

    return iteration_add(minus_one(x), plus_one(y))

def recursive_add(x, y):
    if x == 0:
        return y

    return plus_one(recursive_add(minus_one(x), y))

def minus_one(x):
    return x - 1

def plus_one(x):
    return x + 1