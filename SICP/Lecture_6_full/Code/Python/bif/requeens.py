## Eight Queens Problem, Recursive, Built-in functions
import copy

def solveall(n):
    solutions = fillrows(n, n)
    return solutions if solutions != [] else None

def fillrows(size, row):
    if row == 0:
        return [emptyboard()]

    return filter(lambda poses: issafe(poses), 
                  flatmap(lambda restposes: [adjoinpos(row, col, restposes) for col in range(1, size + 1)], 
                          fillrows(size, row - 1)))

def solveall2(n):
    solutions = fillrows2(n, n)
    return solutions if solutions != [] else None

def fillrows2(size, row):
    if row == 0:
        return [emptyboard()]

    return filter(lambda poses: issafe(poses), 
                  flatmap(lambda col: [adjoinpos(row, col, restposes) for restposes in fillrows2(size, row - 1)], 
                          range(1, size + 1)))

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

def flatten2(lst2layers):
    return [y for x in lst2layers for y in x]

def flatmap(proc, lst2layers):
    return flatten2(map(proc, lst2layers))

## Tests
def test_flatten2():
    assert [1, 2, 3, 4, 5] == flatten2([[1, 2], [3, 4], [5]])
    assert [] == flatten2([])
    assert [] == flatten2([[]])

    print 'test_flatten2 ok'

def test_flatmap():
    assert [1, 2, 3, 2, 4, 6] == flatmap(lambda x: [x, x * 2, x * 3], [1, 2])

    print 'test_flatmap ok'

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
    test_flatten2()
    test_flatmap()

    test_solveall()
    test_solveall2()

    print 'test ok'

if __name__ == '__main__':
    test()