package hod.javacatchup;

import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.IntStream;

public class Euler72 {

    private static final AtomicLong counter = new AtomicLong();

    private static int gcdEuclid2(int a, int b) {
        var max = Math.max(a, b);
        var min = Math.min(a, b);
        var remainder = max % min;
        while (remainder != 0) {
            max = min;
            min = remainder;
            remainder = max % min;
        }
        return min;
    }

    public static boolean isReducedProperFraction(int n, int d) {
        return gcdEuclid2(n, d) == 1;
    }

    public static void main(String[] args) {
        var limit = 8;
        var limit2 = 1000000;
        var max = limit2;
        var ds = numberIterator(1, max);
        var solution = ds.parallel()
                .mapToLong(d -> {
                            counter.incrementAndGet();
                            if (counter.get() % 1000 == 0) {
                                System.out.println(counter.get());
                            }
                            return numberIterator(1, d - 1)
                                    .filter(n -> isReducedProperFraction(n, d))
                                    .count();
                        }
                ).reduce(Long::sum);
        System.out.println("solution = " + solution);
    }

    private static IntStream numberIterator(final int from, final int to) {
        return IntStream.range(from, to + 1);
    }
}
