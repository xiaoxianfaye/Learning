package fayelab.sicp.stream.delaystreamclass;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static java.util.Arrays.asList;
import static fayelab.sicp.stream.delaystreamclass.StreamOp.*;

public class ReQueens
{
    public static Optional<List<List<List<Integer>>>> solveAll(int n)
    {
        List<List<List<Integer>>> solutions = collectStream(fillRows(n, n));
        return solutions.isEmpty() ? Optional.empty() : Optional.of(solutions);
    }
    
    private static Stream<List<List<Integer>>> fillRows(int size, int row)
    {
        if(row == 0)
        {
            return consStream(emptyBoard(), () -> theEmptyStream());
        }
        
        return filterStream((List<List<Integer>> poses) -> isSafe(poses), 
                            flatmap((List<List<Integer>> restPoses) -> mapStream((Integer col) -> adjoinPos(row, col, restPoses), 
                                                                                 enumInterval(1, size)), 
                                    fillRows(size, row - 1)));
    }
    
    public static Optional<List<List<List<Integer>>>> solveAll2(int n)
    {
        List<List<List<Integer>>> solutions = collectStream(fillRows2(n, n));
        return solutions.isEmpty() ? Optional.empty() : Optional.of(solutions);
    }
    
    private static Stream<List<List<Integer>>> fillRows2(int size, int row)
    {
        if(row == 0)
        {
            return consStream(emptyBoard(), () -> theEmptyStream());
        }
        
        return filterStream((List<List<Integer>> poses) -> isSafe(poses), 
                            flatmap((Integer col) -> mapStream((List<List<Integer>> restPoses) -> adjoinPos(row, col, restPoses), 
                                                                fillRows2(size, row - 1)),
                                    enumInterval(1, size)));
    }

    private static List<List<Integer>> emptyBoard()
    {
        return new ArrayList<>();
    }

    private static List<List<Integer>> adjoinPos(int curRow, int curCol, List<List<Integer>> curPoses)
    {
        List<List<Integer>> result = new ArrayList<>();
        result.addAll(curPoses);
        result.add(asList(curRow, curCol));
        return result;
    }
    
    private static boolean isSafe(List<List<Integer>> poses)
    {
        List<Integer> newPos = poses.get(poses.size() - 1);
        List<List<Integer>> restPoses = poses.subList(0, poses.size() - 1);
        return isNotSameCol(newPos, restPoses) && isNotDiagonal(newPos, restPoses);
    }

    private static boolean isNotSameCol(List<Integer> newPos, List<List<Integer>> restPoses)
    {
        return restPoses.stream()
                        .noneMatch(restPos -> newPos.get(1) == restPos.get(1));
    }

    private static boolean isNotDiagonal(List<Integer> newPos, List<List<Integer>> restPoses)
    {
        return restPoses.stream()
                        .noneMatch(restPos -> Math.abs(newPos.get(0) - restPos.get(0)) 
                                                == Math.abs(newPos.get(1) - restPos.get(1)));
    }
}
