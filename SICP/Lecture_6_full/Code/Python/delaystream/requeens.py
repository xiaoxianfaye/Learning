## Eight Queens Problem, Recursive, list stream
import copy

from stream import *

def solveall(n):
    solutions = collect_stream(fillrows(n, n))
    return solutions if solutions != [] else None

def fillrows(size, row):
    if row == 0:
        return cons_stream(emptyboard(), lambda : the_empty_stream())

    return filter_stream(lambda poses: issafe(poses), 
                         flatmap(lambda restposes: map_stream(lambda col: adjoinpos(row, col, restposes), 
                                                              enum_interval(1, size)), 
                                         fillrows(size, row - 1)))

def solveall2(n):
    solutions = collect_stream(fillrows2(n, n))
    return solutions if solutions != [] else None

def fillrows2(size, row):
    if row == 0:
        return cons_stream(emptyboard(), lambda : the_empty_stream())

    return filter_stream(lambda poses: issafe(poses), 
                         flatmap(lambda col: map_stream(lambda restposes: adjoinpos(row, col, restposes), 
                                                        fillrows2(size, row - 1)), 
                                 enum_interval(1, size)))

def emptyboard():
    return [];

def adjoinpos(currow, curcol, curposes):
    newposes = copy.copy(curposes)
    newposes.append((currow, curcol))
    return newposes
    
def issafe(poses):
    newpos = poses[-1]
    restposes = poses[:-1]
    return is_not_samecol(newpos, restposes) and is_not_diagonal(newpos, restposes)

def is_not_samecol(newpos, restposes):
    return all(map(lambda restpos: newpos[1] != restpos[1], restposes))

def is_not_diagonal(newpos, restposes):
    return all(map(lambda restpos: abs(newpos[0] - restpos[0]) != abs(newpos[1] - restpos[1]), restposes))

## Tests
def test_solveall():
    assert [[(1, 2), (2, 4), (3, 1), (4, 3)],
            [(1, 3), (2, 1), (3, 4), (4, 2)]] == solveall(4)
    assert None == solveall(3)
    assert 92 == len(solveall(8))

    print 'test_solveall ok'

def test_solveall2():
    assert [[(1, 3), (2, 1), (3, 4), (4, 2)],
            [(1, 2), (2, 4), (3, 1), (4, 3)]] == solveall2(4)
    assert None == solveall2(3)
    # assert 92 == len(solveall2(8))

    print 'test_solveall2 ok'    

def test():
    test_solveall()
    test_solveall2()

    print 'test ok'

if __name__ == '__main__':
    test()