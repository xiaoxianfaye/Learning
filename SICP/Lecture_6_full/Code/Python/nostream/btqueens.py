## Eight Queens Problem, Backtracking Search
import copy

def solveone(n):
    solution = emptyboard()
    _solveone(n, 1, 1, solution)
    return solution if solution != [] else None

def _solveone(size, currow, curcol, curposes):
    if currow > size:
        return

    if curcol > size:
        if curposes == []:
            return

        lastpos = curposes.pop()
        _solveone(size, lastpos[0], lastpos[1] + 1, curposes)
        return

    adjoinpos(currow, curcol, curposes)
    if issafe(curposes):
        _solveone(size, currow + 1, 1, curposes)
        return

    curposes.pop()
    _solveone(size, currow, curcol + 1, curposes)

def solveall(n):
    solutions = []
    _solveall(n, 1, 1, emptyboard(), solutions)
    return solutions if solutions != [] else None

def _solveall(size, currow, curcol, curposes, solutions):
    if currow > size:
        solutions.append(copy.copy(curposes))
        lastpos = curposes.pop()
        _solveall(size, lastpos[0], lastpos[1] + 1, curposes, solutions)
        return

    if curcol > size:
        if curposes == []:
            return

        lastpos = curposes.pop()
        _solveall(size, lastpos[0], lastpos[1] + 1, curposes, solutions)
        return

    adjoinpos(currow, curcol, curposes)
    if issafe(curposes):
        _solveall(size, currow + 1, 1, curposes, solutions)
        return

    curposes.pop()
    _solveall(size, currow, curcol + 1, curposes, solutions)

def emptyboard():
    return [];

def adjoinpos(currow, curcol, curposes):
    curposes.append((currow, curcol))

def issafe(poses):
    newpos = poses[-1]
    restposes = poses[:-1]
    return is_not_samecol(newpos, restposes) and is_not_diagonal(newpos, restposes)

def is_not_samecol(newpos, restposes):
    return all(map(lambda restpos: newpos[1] != restpos[1], restposes))

def is_not_diagonal(newpos, restposes):
    return all(map(lambda restpos: abs(newpos[0] - restpos[0]) != abs(newpos[1] - restpos[1]), restposes))

## Tests
def test_solveone():
    assert [(1, 2), (2, 4), (3, 1), (4, 3)] == solveone(4)
    assert None == solveone(3)
    assert [(1, 1), (2, 5), (3, 8), (4, 6), 
            (5, 3), (6, 7), (7, 2), (8, 4)] == solveone(8)

    print 'test_solveone ok'

def test_solveall():
    assert [[(1, 2), (2, 4), (3, 1), (4, 3)],
            [(1, 3), (2, 1), (3, 4), (4, 2)]] == solveall(4)
    assert None == solveall(3)
    # assert 92 == len(solveall(8))

    print 'test_solveall ok'

def test():
    test_solveone()
    test_solveall()

    print 'test ok'

if __name__ == '__main__':
    test()