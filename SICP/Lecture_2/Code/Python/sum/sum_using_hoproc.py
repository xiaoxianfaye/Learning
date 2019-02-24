def _sum(_term, a, _next, b, step=1):
    def _reduce_func(acc, _x):
        acc.append(_next(acc[-1]))
        return acc

    result = 0
    seq = reduce(_reduce_func, range(a, b, step), [a])

    for item in seq:
        if item > b:
            break;
        result += _term(item)
    return result

def _sum_r(_term, a, _next, b):
    return 0 if a > b else _term(a) + _sum_r(_term, _next(a), _next, b)

def _sum_tr(_term, a, _next, b):
    return _inner_sum_tr(_term, a, _next, b, 0)

def _inner_sum_tr(_term, a, _next, b, acc):
    return acc if a > b else _inner_sum_tr(_term, _next(a), _next, b, acc + _term(a))

def sum_primitive(a, b):
    return _sum(lambda x: x, a, lambda x: x + 1, b)

def sum_primitive_r(a, b):
    return _sum_r(lambda x: x, a, lambda x: x + 1, b)

def sum_primitive_tr(a, b):
    return _sum_tr(lambda x: x, a, lambda x: x + 1, b)

def sum_square(a, b):
    return _sum(lambda x: x * x, a, lambda x: x + 1, b)

def sum_square_r(a, b):
    return _sum_r(lambda x: x * x, a, lambda x: x + 1, b)

def sum_square_tr(a, b):
    return _sum_tr(lambda x: x * x, a, lambda x: x + 1, b)

def sum_pi(a, b):
    return _sum(lambda x: 1.0 / (x * (x + 2.0)), a, lambda x: x + 4, b, 4)

def sum_pi_r(a, b):
    return _sum_r(lambda x: 1.0 / (x * (x + 2.0)), a, lambda x: x + 4, b)

def sum_pi_tr(a, b):
    return _sum_tr(lambda x: 1.0 / (x * (x + 2.0)), a, lambda x: x + 4, b)