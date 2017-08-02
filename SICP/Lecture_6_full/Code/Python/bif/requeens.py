## Eight Queens Problem, Recursive, Built-in functions
def solveall(n):
    solutions = fillrows(n, n)
    return solutions if solutions != [] else None

def fillrows(size, row):
    if row == 0:
        return [emptyboard()]

    return filter(lambda poses: issafe(poses), 
                  flatten2([adjoinpos(row, col, restposes) for col in range(1, size + 1) for restposes in fillrows(size, row - 1)]))

    # map(lambda col: adjoinpos(row, col, restposes), range(1, size + 1))

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

def flatten2(lst2layers):
    return [y for x in lst2layers for y in x]

## Tests
def test_solveall():
    assert [[(1, 2), (2, 4), (3, 1), (4, 3)],
            [(1, 3), (2, 1), (3, 4), (4, 2)]] == solveall(4)
    # assert None == solveall(3)
    # assert 92 == len(solveall(8))

    print 'test_solveall ok'

def test_solveall2():
    assert [[(1, 3), (2, 1), (3, 4), (4, 2)],
            [(1, 2), (2, 4), (3, 1), (4, 3)]] == solveall2(4)
    assert None == solveall2(3)
    # assert 92 == len(solveall(8))

    print 'test_solveall2 ok'    

def test():
    test_solveall()
    test_solveall2()

    print 'test ok'

if __name__ == '__main__':
    test()