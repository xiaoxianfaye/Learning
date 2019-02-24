def sum_primitive(a, b):
    result = 0
    for i in range(a, b + 1):
        result = result + i
    return result

def sum_primitive_r(a, b):
    return 0 if a > b else a + sum_primitive_r(a + 1, b)

def sum_primitive_tr(a, b):
    return _sum_primitive_tr(a, b, 0)

def _sum_primitive_tr(a, b, acc):
    return acc if a > b else _sum_primitive_tr(a + 1, b, a + acc)

def sum_square(a, b):
    result = 0
    for i in range(a, b + 1):
        result = result + i * i
    return result

def sum_square_r(a, b):
    return 0 if a > b else a * a + sum_square_r(a + 1, b)

def sum_square_tr(a, b):
    return _sum_square_tr(a, b, 0)

def _sum_square_tr(a, b, acc):
    return acc if a > b else _sum_square_tr(a + 1, b, a * a + acc)

def sum_pi(a, b):
    result = 0
    for i in range(a, b + 1, 4):
        result = result + 1.0 / (i * (i + 2.0))
    return result

def sum_pi_r(a, b):
    return 0 if a > b else 1.0 / (a * (a + 2.0)) + sum_pi_r(a + 4, b)

def sum_pi_tr(a, b):
    return _sum_pi_tr(a, b, 0)

def _sum_pi_tr(a, b, acc):
    return acc if a > b else _sum_pi_tr(a + 4, b, 1.0 / (a * (a + 2.0)) + acc)