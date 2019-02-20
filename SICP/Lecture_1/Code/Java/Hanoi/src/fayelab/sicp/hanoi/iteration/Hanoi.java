package fayelab.sicp.hanoi.iteration;

import static java.util.Arrays.asList;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
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
        genMovingSeq(initDiskMovings(n, from, to, spare), power2(n) - 1, 1, result);
        return result;
    }

    static List<DiskMoving> initDiskMovings(int n, String from, String to, String spare)
    {
        return IntStream.rangeClosed(1, n)
                        .mapToObj(i -> isOdd(i)
                                    ? new DiskMoving(i, power2(i - 1), power2(i), from, to, spare)
                                    : new DiskMoving(i, power2(i - 1), power2(i), from, spare, to))
                        .collect(Collectors.toList());
    }

    private static void genMovingSeq(List<DiskMoving> diskMovings, int totalSteps, int curStepNo, List<List<String>> result)
    {
        genCurMovingStep(curStepNo, diskMovings).ifPresent(
                curMovingStep -> move(curMovingStep.getDiskNo(), curMovingStep.getFrom(), curMovingStep.getTo(), result));

        if(curStepNo < totalSteps)
        {
            genMovingSeq(diskMovings, totalSteps, curStepNo + 1, result);
        }
    }

    private static Optional<MovingStep> genCurMovingStep(int curStepNo, List<DiskMoving> diskMovings)
    {
        return diskMovings.stream()
                          .filter(diskMoving -> diskMoving.getStepNo() == curStepNo)
                          .findFirst()
                          .flatMap(curDiskMoving -> {
                              updateDiskMovings(diskMovings, curDiskMoving);
                              return Optional.of(new MovingStep(curDiskMoving.getDiskNo(), curDiskMoving.getFrom(),
                                                      curDiskMoving.getTo(), curDiskMoving.getSpare()));
                          });
    }

    private static void updateDiskMovings(List<DiskMoving> diskMovings, DiskMoving curDiskMoving)
    {
        diskMovings.remove(curDiskMoving);
        diskMovings.add(new DiskMoving(curDiskMoving.getDiskNo(), curDiskMoving.getStepNo() + curDiskMoving.getSteps(),
                curDiskMoving.getSteps(), curDiskMoving.getTo(), curDiskMoving.getSpare(), curDiskMoving.getFrom()));
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

class DiskMoving
{
    private int diskNo;
    private int stepNo;
    private int steps;
    private String from;
    private String to;
    private String spare;

    public DiskMoving(int diskNo, int stepNo, int steps, String from, String to, String spare)
    {
        this.diskNo = diskNo;
        this.stepNo = stepNo;
        this.steps = steps;
        this.from = from;
        this.to = to;
        this.spare = spare;
    }

    public int getDiskNo()
    {
        return diskNo;
    }

    public int getStepNo()
    {
        return stepNo;
    }

    public int getSteps()
    {
        return steps;
    }

    public String getFrom()
    {
        return from;
    }

    public String getTo()
    {
        return to;
    }

    public String getSpare()
    {
        return spare;
    }
}

class MovingStep
{
    private int diskNo;
    private String from;
    private String to;
    private String spare;

    public MovingStep(int diskNo, String from, String to, String spare)
    {
        this.diskNo = diskNo;
        this.from = from;
        this.to = to;
        this.spare = spare;
    }

    public int getDiskNo()
    {
        return diskNo;
    }

    public String getFrom()
    {
        return from;
    }

    public String getTo()
    {
        return to;
    }

    public String getSpare()
    {
        return spare;
    }
}