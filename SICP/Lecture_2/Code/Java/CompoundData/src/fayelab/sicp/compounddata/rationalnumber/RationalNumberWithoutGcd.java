package fayelab.sicp.compounddata.rationalnumber;

import static java.util.Arrays.asList;

import java.util.List;

public class RationalNumberWithoutGcd
{
    public static List<Integer> makeRationalNumber(int numer, int denom)
    {
        return asList(numer, denom);
    }
    
    public static int numer(List<Integer> rationalNumber)
    {
        return head(rationalNumber);
    }

    public static int denom(List<Integer> rationalNumber)
    {
        return tail(rationalNumber);
    }
    
    public static List<Integer> add(List<Integer> rationalNumber1, List<Integer> rationalNumber2)
    {
        return makeRationalNumber(numer(rationalNumber1) * denom(rationalNumber2) + denom(rationalNumber1) * numer(rationalNumber2),
                                  denom(rationalNumber1) * denom(rationalNumber2));
    }
    
    public static List<Integer> mul(List<Integer> rationalNumber1, List<Integer> rationalNumber2)
    {
        return makeRationalNumber(numer(rationalNumber1) * numer(rationalNumber2),
                                  denom(rationalNumber1) * denom(rationalNumber2));
    } 

    private static int head(List<Integer> pair)
    {
        return pair.get(0);
    }
    
    private static int tail(List<Integer> pair)
    {
        return pair.get(1);
    }
}