package hod.javacatchup;

import java.math.BigInteger;

public class Euler57 {

    private static boolean doesTheNumeratorHaveMoreDigits(BigInteger numerator, BigInteger denominator) {
        final boolean ret = numerator.toString().length() > denominator.toString().length();
        if (ret) {
            System.out.println("these are my numbers " + numerator + " and " + denominator);
        }
        return ret;
    }

    //to either add one or two to the rational number
    private static BigInteger addXAndGiveNumeratorBack(int x, BigInteger numerator, BigInteger denominator) {
        return numerator.add(denominator.multiply(BigInteger.valueOf(x)));
    }

    public static int myRecursivePony(int iterations, BigInteger num, BigInteger den, int count) {
        int counting = count;
        if (iterations > 0) {
            num = addXAndGiveNumeratorBack(1, num, den);
            if (doesTheNumeratorHaveMoreDigits(num, den)) {
                counting++;
            }
            num = addXAndGiveNumeratorBack(1, num, den);
            counting = myRecursivePony(iterations - 1, den, num, counting);
        }
        return counting;
    }

    public static void main(String[] args) {
        final int solution = myRecursivePony(1000,
                                             BigInteger.ONE,
                                             BigInteger.valueOf(2),
                                             0);
        System.out.println(solution);
    }
}