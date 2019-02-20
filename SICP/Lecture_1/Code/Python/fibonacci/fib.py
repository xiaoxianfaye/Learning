def recursive_fibo(n):
    if n < 2:
        return n

    return recursive_fibo(n - 1) + recursive_fibo(n - 2)

def iteration_fibo(n):
    if n < 2:
        return n

    a = 0;
    b = 1;
    for i in range(2, n + 1, 1):
        result = a + b
        a = b
        b = result

    return result

def tail_recursive_fibo(n):
    return _tail_recursive_fibo(n, 0, 1)

def _tail_recursive_fibo(count, a, b):
    if count == 0:
        return a

    return _tail_recursive_fibo(count - 1, b, a + b)