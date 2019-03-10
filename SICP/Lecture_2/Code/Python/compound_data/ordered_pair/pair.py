def make_pair(x, y):
    return lambda index: x if index == 0 else y

def head(p):
    return p(0)

def tail(p):
    return p(1)