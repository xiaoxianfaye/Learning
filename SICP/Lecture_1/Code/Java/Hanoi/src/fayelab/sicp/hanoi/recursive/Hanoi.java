package fayelab.sicp.hanoi.recursive;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static java.util.Arrays.asList;

public class Hanoi
{
    public static void run(int n, String from, String to, String spare)
    {
        if(n == 0)
        {
            return;
        }
        
        run(n - 1, from, spare, to);
        move(n, from, to);
        run(n - 1, spare, to, from);
    }

    private static void move(int num, String from, String to)
    {
        System.out.println(String.format("%d: %s -> %s", num, from, to));        
    }

    public static void run2(int n, String from, String to, String spare)
    {
        List<List<String>> movingSeq = genMovingSeq(n, from, to, spare);
        List<String> formattedMovingSeq = formatMovingSeq(movingSeq);
        formattedMovingSeq.forEach(formattedMovingStep -> System.out.println(formattedMovingStep));
    }

    static List<List<String>> genMovingSeq(int n, String from, String to, String spare)
    {
        List<List<String>> result = new ArrayList<>();
        genMovingSeq(n, from, to, spare, result);
        return result;
    }

    private static void genMovingSeq(int n, String from, String to, String spare, List<List<String>> result)
    {
        if(n == 0)
        {
            return;
        }

        genMovingSeq(n - 1, from, spare, to, result);
        move(n, from, to, result);
        genMovingSeq(n - 1, spare, to, from, result);
    }

    private static void move(int nth, String from, String to, List<List<String>> acc)
    {
        acc.add(asList(String.valueOf(nth), from, to));
    }

    private static List<String> formatMovingSeq(List<List<String>> movingSeq)
    {
        return movingSeq.stream()
                        .map((List<String> movingStep) -> String.format("%s: %s -> %s", movingStep.get(0), movingStep.get(1), movingStep.get(2)))
                        .collect(Collectors.toList());
    }

    public static void main(String[] args)
    {
        Hanoi.run(3, "F", "T", "S");
        System.out.println();
        Hanoi.run2(3, "F", "T", "S");
    }
}
