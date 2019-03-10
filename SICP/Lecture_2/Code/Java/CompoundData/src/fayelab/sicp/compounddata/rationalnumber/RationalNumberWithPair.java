package fayelab.sicp.compounddata.rationalnumber;

import static fayelab.sicp.compounddata.pair.Pair.*;

import java.util.function.Function;

public class RationalNumberWithPair
{
    public static Function<Integer, Integer> makeRationalNumber(int numer, int denom)
    {
        int gcd = gcd(numer, denom);
        return makePair(numer / gcd, denom / gcd);
    }
    
    public static int numer(Function<Integer, Integer> rationalNumber)
    {
        return head(rationalNumber);
    }

    public static int denom(Function<Integer, Integer> rationalNumber)
    {
        return tail(rationalNumber);
    }
    
    public static Function<Integer, Integer> add(Function<Integer, Integer> rationalNumber1, Function<Integer, Integer> rationalNumber2)
    {
        return makeRationalNumber(numer(rationalNumber1) * denom(rationalNumber2) + denom(rationalNumber1) * numer(rationalNumber2),
                                  denom(rationalNumber1) * denom(rationalNumber2));
    }
    
    public static Function<Integer, Integer> mul(Function<Integer, Integer> rationalNumber1, Function<Integer, Integer> rationalNumber2)
    {
        return makeRationalNumber(numer(rationalNumber1) * numer(rationalNumber2),
                                  denom(rationalNumber1) * denom(rationalNumber2));
    }
    
    public static int gcd(int x, int y)
    {
        if(y == 0)
        {
            return x;
        }
        
        return gcd(y, x % y);
    }
}
