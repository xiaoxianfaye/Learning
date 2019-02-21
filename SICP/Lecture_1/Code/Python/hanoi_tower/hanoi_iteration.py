def run(n, src, dest, spare):
    moving_seq = gen_moving_seq(n, src, dest, spare)
    formatted_moving_seq = format_moving_seq(moving_seq)
    for formatted_moving_step in formatted_moving_seq:
        print formatted_moving_step

def gen_moving_seq(n, src, dest, spare):
    result = []
    _gen_moving_seq(init_disk_movings(n, src, dest, spare), power2(n) - 1, 1, result)
    return result

def init_disk_movings(n, src, dest, spare):
    return map(lambda i: DiskMoving(i, power2(i - 1), power2(i), src, dest, spare)
                    if is_odd(i) else DiskMoving(i, power2(i - 1), power2(i), src, spare, dest),
               range(1, n + 1))

def _gen_moving_seq(disk_movings, total_steps, cur_stepno, result):
    cur_moving_step = gen_cur_moving_step(cur_stepno, disk_movings)
    if cur_moving_step == None:
        raise RuntimeError('Something wrong happened')

    move(cur_moving_step.diskno, cur_moving_step.src, cur_moving_step.dest, result)

    if cur_stepno < total_steps:
        _gen_moving_seq(disk_movings, total_steps, cur_stepno + 1, result)

def gen_cur_moving_step(cur_stepno, disk_movings):
    filtered_disk_movings = filter(lambda disk_moving: disk_moving.stepno == cur_stepno, disk_movings)

    if len(filtered_disk_movings) < 1:
        return None

    cur_disk_moving = filtered_disk_movings[0]
    update_disk_movings(disk_movings, cur_disk_moving)
    return MovingStep(cur_disk_moving.diskno, cur_disk_moving.src, cur_disk_moving.dest, cur_disk_moving.spare)

def update_disk_movings(disk_movings, cur_disk_moving):
    disk_movings.remove(cur_disk_moving)
    disk_movings.append(DiskMoving(cur_disk_moving.diskno, cur_disk_moving.stepno + cur_disk_moving.steps,
                                   cur_disk_moving.steps, cur_disk_moving.dest, cur_disk_moving.spare,
                                   cur_disk_moving.src))

def move(diskno, src, dest, result):
    result.append([diskno, src, dest])

def power2(n):
    return reduce(lambda x, y: x * y, map(lambda i: 2, range(1, n + 1)), 1)

def is_odd(n):
    return n % 2 == 1

def format_moving_seq(moving_seq):
    return map(lambda moving_step: '%d: %s -> %s' % (moving_step[0], moving_step[1], moving_step[2]), moving_seq)

class DiskMoving:
    def __init__(self, diskno, stepno, steps, src, dest, spare):
        self.diskno = diskno
        self.stepno = stepno
        self.steps = steps
        self.src = src
        self.dest = dest
        self.spare = spare

class MovingStep:
    def __init__(self, diskno, src, dest, spare):
        self.diskno = diskno
        self.src = src
        self.dest = dest
        self.spare = spare

if __name__ == '__main__':
    run(3, 'from', 'to', 'spare')
    print
    run(4, 'from', 'to', 'spare')


