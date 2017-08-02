## Stream with list

# Constructors
def cons_stream(head, tail):
    return [head] + tail

def the_empty_stream():
    return []

# Selectors
def head(s):
    return s[0]

def tail(s):
    return s[1:]

def is_empty_stream(s):
    return s == []

# Stream Operations
def map_stream(proc, s):
    if is_empty_stream(s):
        return the_empty_stream()

    return cons_stream(proc(head(s)), map_stream(proc, tail(s)))

def filter_stream(pred, s):
    if is_empty_stream(s):
        return the_empty_stream()

    if pred(head(s)):
        return cons_stream(head(s), filter_stream(pred, tail(s)))

    return filter_stream(pred, tail(s))

def acc_stream(proc, acc, s):
    if is_empty_stream(s):
        return acc

    return proc(head(s), acc_stream(proc, acc, tail(s)))

def append_stream(s1, s2):
    if is_empty_stream(s1):
        return s2

    return cons_stream(head(s1), append_stream(tail(s1), s2))

def enum_tree(obj):
    if isinstance(obj, tuple):
        return append_stream(enum_tree(obj[0]), enum_tree(obj[1]))

    return cons_stream(obj, the_empty_stream())

def enum_interval(low, high):
    if low > high:
        return the_empty_stream()

    return cons_stream(low, enum_interval(low + 1, high))

def flatten(sos):
    if is_empty_stream(sos):
        return the_empty_stream()

    return acc_stream(lambda s1, s2: append_stream(s1, s2), the_empty_stream(), sos)

def flatmap(proc, sos):
    return flatten(map_stream(proc, sos))

def nonematch(pred, s):
    if is_empty_stream(s):
        return True

    if pred(head(s)):
        return False

    return nonematch(pred, tail(s))

## Tests
def test_cons_stream():
    assert [1, 2, 3] == cons_stream(1, [2, 3])
    assert [1] == cons_stream(1, [])
    print 'test_cons_stream ok'

def test_the_empty_stream():
    assert [] == the_empty_stream()
    print 'test_the_empty_stream ok'

def test_head():
    assert 1 == head([1, 2, 3])
    print 'test_head ok'

def test_tail():
    assert [2, 3] == tail([1, 2, 3])
    print 'test_tail ok'

def test_is_empty_stream():
    assert is_empty_stream([])
    print 'test_is_empty_stream ok'

def test_map_stream():
    assert [2, 4] == map_stream(lambda x: 2 * x, [1, 2])
    assert ['1', '2'] == map_stream(lambda x: str(x), [1, 2])
    assert [] == map_stream(lambda x: x, [])
    print 'test_map_stream ok'

def test_filter_stream():
    assert [2, 4] == filter_stream(lambda x: x % 2 == 0, [1, 2, 3, 4])
    assert [] == filter_stream(lambda x: True, [])
    assert [] == filter_stream(lambda x: False, [])
    print 'test_filter_stream ok'

def test_acc_stream():
    assert 10 == acc_stream(lambda x, y: x + y, 0, [1, 2, 3, 4])
    assert '123' == acc_stream(lambda x, y: ''.join([str(x), str(y)]), '', [1, 2, 3])
    assert 0 == acc_stream(lambda x, y: x + y, 0, [])
    assert 1 == acc_stream(lambda x, y: x + y, 0, [1])
    print 'test_acc_stream ok'

def test_append_stream():
    assert [1, 2, 3, 4] == append_stream([1, 2], [3, 4])
    assert [3, 4] == append_stream([], [3, 4])
    assert [1, 2] == append_stream([1, 2], [])
    assert [] == append_stream([], [])
    
    print 'test_append_stream ok'

def test_enum_tree():
    assert [1, 2, 7, 19, 12, 14] == enum_tree(((1, (2, 7)), (19, (12, 14))))
    assert [1, 2, 3, 7, 19, 12, 13, 14] == enum_tree(((1, ((2, 3), 7)), (19, (12, (13, 14)))))
    print 'test_enum_tree ok'

def test_enum_interval():
    assert [2, 3, 4, 5] == enum_interval(2, 5)
    assert [] == enum_interval(3, 1)
    print 'test_enum_interval ok'

def test_flatten():
    assert [1, 2, 3, 4, 5] == flatten([[1, 2], [3, 4], [5]])
    assert [1, 2] == flatten([[1, 2]])
    assert [] == flatten([])

    print 'test_flatten ok'

def test_flatmap():
    assert [1, 2, 3, 2, 4, 6] == flatmap(lambda x: [x, x * 2, x * 3], [1, 2])
    assert [] == flatmap(lambda x: [x], [])

    print 'test_flatmap ok'

def test_nonematch():
    assert nonematch(lambda x: x % 2 == 0, [1, 3, 5])
    assert nonematch(lambda x: True, [])
    assert nonematch(lambda x: False, [])

    print 'test_nonematch ok'

def test():
    test_cons_stream()
    test_the_empty_stream()
    test_head()
    test_tail()
    test_is_empty_stream()
    test_map_stream()
    test_filter_stream()
    test_acc_stream()
    test_append_stream()
    test_enum_tree()
    test_enum_interval()
    test_flatten()
    test_flatmap()
    test_nonematch()

    print 'test ok'

if __name__ == '__main__':
    test()
