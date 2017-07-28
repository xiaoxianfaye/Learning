package fayelab.sicp.stream.nostream;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static java.util.Arrays.*;

public class BtQueens
{
    public static Optional<List<List<Integer>>> solveOne(int n)
    {
        List<List<Integer>> solution = emptyBoard();
        solveOne(n, 1, 1, solution);
        return solution.isEmpty() ? Optional.empty() : Optional.of(solution);
    }

    private static void solveOne(int size, int curRow, int curCol, List<List<Integer>> curPoses)
    {
        if(curRow > size)
        {
            return;
        }
        
        if(curCol > size)
        {
            if(curPoses.isEmpty())
            {
                return;
            }
            
            List<Integer> lastPos = curPoses.remove(curPoses.size() - 1);
            solveOne(size, lastPos.get(0), lastPos.get(1) + 1, curPoses);
            return;
        }
        
        adjoinPos(curRow, curCol, curPoses);
        if(isSafe(curPoses))
        {
            solveOne(size, curRow + 1, 1, curPoses);
            return;
        }
        
        curPoses.remove(curPoses.size() - 1);
        solveOne(size, curRow, curCol + 1, curPoses);
    }
    
    public static Optional<List<List<List<Integer>>>> solveAll(int n)
    {
        List<List<List<Integer>>> solutions = new ArrayList<>();
        solveAll(n, 1, 1, emptyBoard(), solutions);
        return solutions.isEmpty() ? Optional.empty() : Optional.of(solutions);
    }

    private static void solveAll(int size, int curRow, int curCol, 
            List<List<Integer>> curPoses, List<List<List<Integer>>> solutions)
    {
        if(curRow > size)
        {
            solutions.add(new ArrayList<>(curPoses));
            List<Integer> lastPos = curPoses.remove(curPoses.size() - 1);
            solveAll(size, lastPos.get(0), lastPos.get(1) + 1, curPoses, solutions);
            return;
        }
        
        if(curCol > size)
        {
            if(curPoses.isEmpty())
            {
                return;
            }
            
            List<Integer> lastPos = curPoses.remove(curPoses.size() - 1);
            solveAll(size, lastPos.get(0), lastPos.get(1) + 1, curPoses, solutions);
            return;
        }
        
        adjoinPos(curRow, curCol, curPoses);
        if(isSafe(curPoses))
        {
            solveAll(size, curRow + 1, 1, curPoses, solutions);
            return;
        }
        
        curPoses.remove(curPoses.size() - 1);
        solveAll(size, curRow, curCol + 1, curPoses, solutions);
    }
    
    private static List<List<Integer>> emptyBoard()
    {
        return new ArrayList<>();
    }

    private static void adjoinPos(int curRow, int curCol, List<List<Integer>> curPoses)
    {
        curPoses.add(asList(curRow, curCol));
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
