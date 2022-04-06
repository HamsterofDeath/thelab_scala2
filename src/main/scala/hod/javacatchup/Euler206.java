package hod.javacatchup;

import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Euler206 {

    private static final String pattern   = "1_2_3_4_5_6_7_8_9_0";
    private static final String reference = pattern.replaceAll("_", "");
    private static       int    counter   = 0;

    record Tuple(int digit, int index) {
    }

    private static boolean matches(BigInteger bi) {
        if (counter++ % 1000000 == 0) {
            System.out.print('.');
        }
        final AtomicInteger index = new AtomicInteger();
        var relevantDigits = bi.toString()
                .chars()
                .boxed()
                .map(e -> new Tuple(Character.getNumericValue(e), index.getAndIncrement()))
                .filter(tuple -> tuple.index % 2 == 0)
                .map(e -> String.valueOf(e.digit))
                .collect(Collectors.joining());
        return relevantDigits.equalsIgnoreCase(reference);
    }

    public static void main(String[] args) {
        var max = new BigInteger(pattern.replace('_', '9'));
        var min = new BigInteger(pattern.replace('_', '0'));
        var minRoot = min.sqrt();
        var maxRoot = max.sqrt();
        var candidates =
                Stream.iterate(minRoot,
                               bi -> bi.compareTo(maxRoot) <= 0,
                               bi -> bi.add(BigInteger.ONE));
        var solution =
                candidates.map(e -> e.pow(2))
                        .filter(Euler206::matches)
                        .findFirst()
                        .map(BigInteger::sqrt);
        System.out.println(solution);
    }
}
