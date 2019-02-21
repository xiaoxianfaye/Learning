def run(n, src, dest, spare):
    moving_seq = gen_moving_seq(n, src, dest, spare)
    formatted_moving_seq = format_moving_seq(moving_seq)
    for formatted_moving_step in formatted_moving_seq:
        print formatted_moving_step

def gen_moving_seq(n, src, dest, spare):
    result = []
    _gen_moving_seq(power2(n) - 1, 1, gen_even_odd_movers(n, src, dest, spare), result)
    return result

def _gen_moving_seq(total_steps, cur_stepno, even_odd_movers, result):
    (diskno, circular_stepno) = calc_diskno_and_circular_stepno(cur_stepno)
    (even_mover, odd_mover) = even_odd_movers
    moving = odd_mover(circular_stepno) if is_odd(diskno) else even_mover(circular_stepno)
    move(diskno, moving[0], moving[1], result)

    if cur_stepno < total_steps:
        _gen_moving_seq(total_steps, cur_stepno + 1, even_odd_movers, result)

def calc_diskno_and_circular_stepno(cur_stepno):
    return _calc_diskno_and_circular_stepno(cur_stepno, 0)

def _calc_diskno_and_circular_stepno(cur_stepno, count):
    return (count + 1, (cur_stepno + 1) / 2 % 3) \
                if is_odd(cur_stepno) else _calc_diskno_and_circular_stepno(cur_stepno / 2, count + 1)

def gen_even_odd_movers(cur_stepno, src, dest, spare):
    return (gen_mover(src, spare, dest), gen_mover(src, dest, spare)) \
                if is_odd(cur_stepno) else (gen_mover(src, dest, spare), gen_mover(src, spare, dest))

def gen_mover(src, dest, spare):
    def gen(circular_stepno):
        if circular_stepno == 1:
            return [src, dest]
        elif circular_stepno == 2:
            return [dest, spare]
        elif circular_stepno == 0:
            return [spare, src]
        else:
            raise RuntimeError('Something wrong happened')

    return gen

def move(diskno, src, dest, result):
    result.append([diskno, src, dest])

def power2(n):
    return reduce(lambda x, y: x * y, map(lambda i: 2, range(1, n + 1)), 1)

def is_odd(n):
    return n % 2 == 1

def format_moving_seq(moving_seq):
    return map(lambda moving_step: '%d: %s -> %s' % (moving_step[0], moving_step[1], moving_step[2]), moving_seq)

if __name__ == '__main__':
    run(3, 'from', 'to', 'spare')
    print
    run(4, 'from', 'to', 'spare')