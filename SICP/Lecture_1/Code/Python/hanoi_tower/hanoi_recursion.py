def run(n, src, dest, spare):
    moving_seq = gen_moving_seq(n, src, dest, spare)
    formatted_moving_seq = format_moving_seq(moving_seq)
    for formatted_moving_step in formatted_moving_seq:
        print formatted_moving_step

def gen_moving_seq(n, src, dest, spare):
    result = []
    _gen_moving_seq(n, src, dest, spare, result)
    return result

def _gen_moving_seq(n, src, dest, spare, result):
    if n == 0:
        return

    _gen_moving_seq(n - 1, src, spare, dest, result)
    move(n, src, dest, result)
    _gen_moving_seq(n - 1, spare, dest, src, result)

def move(nth, src, dest, result):
    result.append([nth, src, dest])

def format_moving_seq(moving_seq):
    return map(lambda moving_step: '%d: %s -> %s' % (moving_step[0], moving_step[1], moving_step[2]), moving_seq)

if __name__ == '__main__':
    run(3, 'from', 'to', 'spare')
    print
    run(4, 'from', 'to', 'spare')