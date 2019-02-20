package fayelab.sicp.hanoi.iterconst;

import static java.util.Arrays.asList;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Hanoi
{
    public static void run(int n, String from, String to, String spare)
    {
        List<List<String>> movingSeq = genMovingSeq(n, from, to, spare);
        List<String> formattedMovingSeq = formatMovingSeq(movingSeq);
        formattedMovingSeq.forEach(formattedMovingStep -> System.out.println(formattedMovingStep));
    }

    static List<List<String>> genMovingSeq(int n, String from, String to, String spare)
    {
        List<List<String>> result = new ArrayList<>();
        genMovingSeq(power2(n) - 1, 1, genEvenOddMovers(n, from, to, spare), result);
        return result;
    }

    private static void genMovingSeq(int totalSteps, int curStepNo,
            List<Function<Integer, List<String>>> evenOddMovers, List<List<String>> result)
    {
        List<Integer> diskNoAndCircularStepNo = calcDiskNoAndCircularStepNo(curStepNo);
        int diskNo = diskNoAndCircularStepNo.get(0);
        int circularStepNo = diskNoAndCircularStepNo.get(1);
        Function<Integer, List<String>> evenMover = evenOddMovers.get(0);
        Function<Integer, List<String>> oddMover = evenOddMovers.get(1);

        List<String> moving = isOdd(diskNo) ? oddMover.apply(circularStepNo) : evenMover.apply(circularStepNo);
        move(diskNo, moving.get(0), moving.get(1), result);

        if(curStepNo < totalSteps)
        {
            genMovingSeq(totalSteps, curStepNo + 1, evenOddMovers, result);
        }
    }

    static List<Integer> calcDiskNoAndCircularStepNo(int curStepNo)
    {
        return calcDiskNoAndCircularStepNo(curStepNo, 0);
    }

    private static List<Integer> calcDiskNoAndCircularStepNo(int curStepNo, int count)
    {
        if(isOdd(curStepNo))
        {
            return asList(count + 1, ((curStepNo + 1) / 2) % 3);
        }

        return calcDiskNoAndCircularStepNo(curStepNo / 2, count + 1);
    }

    static List<Function<Integer, List<String>>> genEvenOddMovers(int n, String from, String to, String spare)
    {
        return isOdd(n) ? asList(genMover(from, spare, to), genMover(from, to, spare))
                        : asList(genMover(from, to, spare), genMover(from, spare, to));
    }

    private static Function<Integer, List<String>> genMover(String from, String to, String spare)
    {
        return (Integer circularStepNo) -> {
            switch(circularStepNo)
            {
                case 1:
                    return asList(from, to);
                case 2:
                    return asList(to, spare);
                case 0:
                    return asList(spare, from);
                default:
                    throw new RuntimeException("Something wrong happened.");
            }
        };
    }

    private static void move(int diskNo, String from, String to, List<List<String>> result)
    {
        result.add(asList(String.valueOf(diskNo), from, to));
    }

    static int power2(int n)
    {
        return IntStream.rangeClosed(1, n).map(i -> 2).reduce(1, (x, y) -> x * y);
    }

    static boolean isOdd(int n)
    {
        return n % 2 == 1;
    }

    static List<String> formatMovingSeq(List<List<String>> movingSeq)
    {
        return movingSeq.stream()
                        .map((List<String> movingStep) -> String.format("%s: %s -> %s", movingStep.get(0), movingStep.get(1), movingStep.get(2)))
                        .collect(Collectors.toList());
    }

    public static void main(String[] args)
    {
        Hanoi.run(3, "F", "T", "S");
        System.out.println();
        Hanoi.run(4, "F", "T", "S");
    }
}
