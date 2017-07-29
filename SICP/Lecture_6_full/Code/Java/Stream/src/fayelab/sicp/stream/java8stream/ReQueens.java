package fayelab.sicp.stream.java8stream;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;

public class ReQueens
{
    public static Optional<List<List<List<Integer>>>> solveAll(int n)
    {
        List<List<List<Integer>>> solutions = fillRows(n, n).collect(toList());
        return solutions.isEmpty() ? Optional.empty() : Optional.of(solutions);
    }
    
    private static Stream<List<List<Integer>>> fillRows(int size, int row)
    {
        if(row == 0)
        {
            return Stream.of(emptyBoard());
        }
                
        return fillRows(size, row - 1)
                .flatMap(restPoses -> enumInterval(1, size).map(col -> adjoinPos(row, col, restPoses)))
                .filter(poses -> isSafe(poses));
    }
    
    public static Optional<List<List<List<Integer>>>> solveAll2(int n)
    {
        List<List<List<Integer>>> solutions = fillRows2(n, n).collect(toList());
        return solutions.isEmpty() ? Optional.empty() : Optional.of(solutions);
    }
    
    private static Stream<List<List<Integer>>> fillRows2(int size, int row)
    {
        if(row == 0)
        {
            return Stream.of(emptyBoard());
        }
                
        return enumInterval(1, size).flatMap(col -> fillRows2(size, row - 1).map(restPoses -> adjoinPos(row, col, restPoses)))
                                    .filter(poses -> isSafe(poses));
    }
    
    private static Stream<Integer> enumInterval(int low, int high)
    {
        return IntStream.rangeClosed(low, high).boxed();
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
