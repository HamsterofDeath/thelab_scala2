package hod.javacatchup;

import java.util.stream.IntStream;

public class Euler33 {
    public static void main(String[] args) {
        final int[] to99 = IntStream.range(0, 100).toArray();
        final int[] nonZeroTo99 = IntStream.range(1, 100).toArray();
        final int[] nonZeroTo9 = IntStream.range(1, 10).toArray();
        for (var numerator : to99) {
            for (int denominator : nonZeroTo99) {
                if (numerator < denominator && numerator >= 10) {
                    var actual = numerator / (double) denominator;
                    for (var remove : nonZeroTo9) {
                        var reducedNumerator = removeDigit(numerator, remove);
                        var reducedDenominator = removeDigit(denominator, remove);
                        if (reducedDenominator > 0 && reducedNumerator >= 0) {
                            var reduced = reducedNumerator / (double) reducedDenominator;
                            if (Math.abs(actual - reduced) < 0.00001) {
                                System.out.println(
                                        numerator + "/" + denominator + " = " + reducedNumerator +
                                        "/" + reducedDenominator);
                            }
                        }
                    }
                }
            }
        }
        System.out.println("100 :)");
    }

    private static int removeDigit(final int numerator, final int remove) {
        final String s = String.valueOf(numerator).replaceAll(String.valueOf(remove), "");
        if (s.isEmpty() || s.length() == 2) {
            return -1;
        } else {
            return Integer.parseInt(s);
        }
    }
}
