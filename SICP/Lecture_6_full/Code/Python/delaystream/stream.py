## Delay Stream

# Constructors
def cons_stream(head, tail):
    return [head, tail]

def the_empty_stream():
    return []

# Selectors
def head(s):
    return s[0]

def tail(s):
    return s[1]()

def is_empty_stream(s):
    return s == []

# Stream Operations
def map_stream(proc, s):
    if is_empty_stream(s):
        return the_empty_stream()

    return cons_stream(proc(head(s)), lambda : map_stream(proc, tail(s)))

def filter_stream(pred, s):
    if is_empty_stream(s):
        return the_empty_stream()

    if pred(head(s)):
        return cons_stream(head(s), lambda : filter_stream(pred, tail(s)))

    return filter_stream(pred, tail(s))

def acc_stream(proc, acc, s):
    if is_empty_stream(s):
        return acc

    return proc(head(s), acc_stream(proc, acc, tail(s)))

def append_stream(s1, s2):
    if is_empty_stream(s1):
        return s2

    return cons_stream(head(s1), lambda : append_stream(tail(s1), s2))

def enum_tree(obj):
    if isinstance(obj, tuple):
        return append_stream(enum_tree(obj[0]), enum_tree(obj[1]))

    return cons_stream(obj, lambda : the_empty_stream())

def enum_interval(low, high):
    if low > high:
        return the_empty_stream()

    return cons_stream(low, lambda : enum_interval(low + 1, high))

# def flatten(sos):
#     if is_empty_stream(sos):
#         return the_empty_stream()

#     return acc_stream(lambda s1, s2: append_stream(s1, s2), the_empty_stream(), sos)

def flatten(sos):
    if is_empty_stream(sos):
        return the_empty_stream()

    return _flatten(head(sos), tail(sos))

def _flatten(headS, sos):
    if is_empty_stream(headS):
        return flatten(sos)

    return cons_stream(head(headS), lambda : _flatten(tail(headS), sos))

def flatmap(proc, sos):
    return flatten(map_stream(proc, sos))

def nonematch(pred, s):
    if is_empty_stream(s):
        return True

    if pred(head(s)):
        return False

    return nonematch(pred, tail(s))

def nth_stream(idx, s):
    if is_empty_stream(s):
        raise RuntimeError('This index does not exist.')

    return head(s) if idx == 0 else nth_stream(idx - 1, tail(s))

def print_stream(s):
    if is_empty_stream(s):
        print 'Done.'
        return

    print head(s),
    print_stream(tail(s))

def print_stream_limit(num, s):
    if is_empty_stream(s):
        print 'Done.'
        return

    if num == 0:
        print 'ok.'
        return
    
    print head(s),
    print_stream_limit(num - 1, tail(s))

def integers_from(n):
    return cons_stream(n, lambda : integers_from(n + 1))

def add_stream(s1, s2):
    if is_empty_stream(s1) and is_empty_stream(s2):
        return the_empty_stream()

    if is_empty_stream(s1):
        return s2

    if is_empty_stream(s2):
        return s1

    return cons_stream(head(s1) + head(s2), lambda : add_stream(tail(s1), tail(s2)))

def scale_stream(c, s):
    if is_empty_stream(s):
        return the_empty_stream()

    return cons_stream(c * head(s), lambda : scale_stream(c, tail(s)))

def ones():
    return cons_stream(1, lambda : ones())

def integers():
    return cons_stream(1, lambda : add_stream(integers(), ones()))

def fibs2(n1, n2):
    return cons_stream(n1, lambda : fibs2(n2, n1 + n2))

def fibs():
    return cons_stream(0, lambda : cons_stream(1, lambda : add_stream(fibs(), tail(fibs()))))

## Auxiliary functions
def list_to_stream(lst):
    if lst == []:
        return the_empty_stream()

    head = lst.pop(0)
    return cons_stream(head, lambda : list_to_stream(lst))

def collect_stream(s):
    if is_empty_stream(s):
        return []

    return [head(s)] + collect_stream(tail(s))

def collect_stream_limit(num, s):
    result = []
    _collect_stream_limit(num, s, result)
    return result

def _collect_stream_limit(num, s, result):
    if is_empty_stream(s):
        return

    if num <= 0:
        return

    result.append(head(s))
    _collect_stream_limit(num - 1, tail(s), result)

## Tests
def test_stream_constructors_and_selectors():
    assert [1, 2, 3] == collect_stream(cons_stream(1, lambda : list_to_stream([2, 3])))
    assert [1] == collect_stream(cons_stream(1, lambda: list_to_stream([])))
    assert [] == collect_stream(the_empty_stream())
    print 'test_stream_constructors_and_selectors ok'

def test_map_stream():
    assert [2, 4] == collect_stream(map_stream(lambda x: 2 * x, list_to_stream([1, 2])))
    assert ['1', '2'] == collect_stream(map_stream(lambda x: str(x), list_to_stream([1, 2])))
    assert [] == collect_stream(map_stream(lambda x: x, list_to_stream([])))
    print 'test_map_stream ok'

def test_filter_stream():
    assert [2, 4] == collect_stream(filter_stream(lambda x: x % 2 == 0, list_to_stream([1, 2, 3, 4])))
    assert [] == collect_stream(filter_stream(lambda x: True, list_to_stream([])))
    assert [] == collect_stream(filter_stream(lambda x: False, list_to_stream([])))
    print 'test_filter_stream ok'

def test_acc_stream():
    assert 10 == acc_stream(lambda x, y: x + y, 0, list_to_stream([1, 2, 3, 4]))
    assert '123' == acc_stream(lambda x, y: ''.join([str(x), str(y)]), '', list_to_stream([1, 2, 3]))
    assert 0 == acc_stream(lambda x, y: x + y, 0, list_to_stream([]))
    assert 1 == acc_stream(lambda x, y: x + y, 0, list_to_stream([1]))
    print 'test_acc_stream ok'

def test_append_stream():
    assert [1, 2, 3, 4] == collect_stream(append_stream(list_to_stream([1, 2]), list_to_stream([3, 4])))
    assert [3, 4] == collect_stream(append_stream(list_to_stream([]), list_to_stream([3, 4])))
    assert [1, 2] == collect_stream(append_stream(list_to_stream([1, 2]), list_to_stream([])))
    assert [] == collect_stream(append_stream(list_to_stream([]), list_to_stream([])))
    
    print 'test_append_stream ok'

def test_enum_tree():
    assert [1, 2, 7, 19, 12, 14] == collect_stream(enum_tree(((1, (2, 7)), (19, (12, 14)))))
    assert [1, 2, 3, 7, 19, 12, 13, 14] == collect_stream(enum_tree(((1, ((2, 3), 7)), (19, (12, (13, 14))))))
    print 'test_enum_tree ok'

def test_enum_interval():
    assert [2, 3, 4, 5] == collect_stream(enum_interval(2, 5))
    assert [] == collect_stream(enum_interval(3, 1))
    print 'test_enum_interval ok'

def test_flatten():
    assert [1, 2, 3, 4, 5] == collect_stream(flatten(list_to_stream([list_to_stream([1, 2]), 
                                                                     list_to_stream([3, 4]), 
                                                                     list_to_stream([5])])))
    assert [1, 2] == collect_stream(flatten(list_to_stream([list_to_stream([1, 2])])))
    assert [] == collect_stream(flatten(list_to_stream([])))

    print 'test_flatten ok'

def test_flatmap():
    assert [1, 2, 3, 2, 4, 6] == collect_stream(flatmap(lambda x: list_to_stream([x, x * 2, x * 3]), 
                                                list_to_stream([1, 2])))
    assert [] == collect_stream(flatmap(lambda x: [x], list_to_stream([])))

    print 'test_flatmap ok'

def test_nonematch():
    assert nonematch(lambda x: x % 2 == 0, list_to_stream([1, 3, 5]))
    assert nonematch(lambda x: True, list_to_stream([]))
    assert nonematch(lambda x: False, list_to_stream([]))

    print 'test_nonematch ok'

def test_nth_stream():
    s = list_to_stream([0, 1])
    assert 0 == nth_stream(0, s)
    assert 1 == nth_stream(1, s)

    try:
        nth_stream(2, s)
        assert False
    except RuntimeError as e:
        assert 'This index does not exist.' == e.message
        assert True

    print 'test_nth_stream ok'

def test_print_stream():
    print_stream(list_to_stream([0, 1]));

    print 'test_print_stream ok'

def test_integers_from():
    s = integers_from(2)
    assert 2 == nth_stream(0, s)
    assert 3 == nth_stream(1, s)
        
    s2 = integers_from(0)
    assert [0, 1, 2] == collect_stream_limit(3, s2)   
    assert [] == collect_stream_limit(-1, s2)

    assert [] == collect_stream_limit(-1, the_empty_stream())

    print 'test_integers_from ok'

def test_add_stream():
    assert [4, 6] == collect_stream(add_stream(enum_interval(1, 2), enum_interval(3, 4)))
    assert [] == collect_stream(add_stream(the_empty_stream(), the_empty_stream()))
    assert [1, 2] == collect_stream(add_stream(enum_interval(1, 2), the_empty_stream()))
    assert [3, 4] == collect_stream(add_stream(the_empty_stream(), enum_interval(3, 4)))
    assert [4, 4] == collect_stream(add_stream(enum_interval(1, 1), enum_interval(3, 4)))
    assert [4, 2] == collect_stream(add_stream(enum_interval(1, 2), enum_interval(3, 3)))

    assert [3, 5, 7, 9] == collect_stream_limit(4, add_stream(integers_from(1), integers_from(2)))

    print 'test_add_stream ok'

def test_scale_stream():
    assert [2, 4] == collect_stream(scale_stream(2, enum_interval(1, 2)))
    assert [] == collect_stream(scale_stream(2, the_empty_stream()))

    assert [2, 4, 6, 8] == collect_stream_limit(4, scale_stream(2, integers_from(1)))

    print 'test_scale_stream ok'

def test_ones():
    assert [1, 1, 1, 1] == collect_stream_limit(4, ones())

    print 'test_ones ok'
    
def test_integers():
    assert [1, 2, 3, 4] == collect_stream_limit(4, integers())

    print 'test_integers ok'

def test_fibs():
    assert [0, 1, 1, 2, 3, 5] == collect_stream_limit(6, fibs2(0, 1))
    assert [1, 2, 3, 5, 8, 13] == collect_stream_limit(6, fibs2(1, 2))

    assert [0, 1, 1, 2, 3, 5] == collect_stream_limit(6, fibs())

    print 'test_fibs ok'

def test():
    test_stream_constructors_and_selectors()
    test_map_stream()
    test_filter_stream()
    test_acc_stream()
    test_append_stream()
    test_enum_tree()
    test_enum_interval()
    test_flatten()
    test_flatmap()
    test_nonematch()
    test_nth_stream()
    test_print_stream()
    test_integers_from()
    test_add_stream()
    test_scale_stream()
    test_ones()
    test_integers()
    test_fibs()

    print 'test ok'

if __name__ == '__main__':
    test()
