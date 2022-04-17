package hod.javacatchup;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Euler27 {
    public static void main(String[] args) {
        final int limit = 1000;
        var maxPrimeCount = 0;
        var coEfficient = 0;
        final int[] as = IntStream.range(-limit + 1, limit).toArray();
        final int[] bs = IntStream.range(-limit, limit + 1).toArray();
        for (var a : as) {
            for (var b : bs) {
                var count = primeCountOf(a, b);

                if (count > maxPrimeCount) {
                    coEfficient = a * b;
                    System.out.println("a = " + a + ",b = " + b + ", primes = " + count + ", coeff: " + coEfficient);
                    maxPrimeCount = count;
                }
            }
        }
        System.out.println(coEfficient);
    }

    private static int primeCountOf(int a, int b) {
        final Stream<Integer> stream = Stream.iterate(0, e -> e + 1)
                .map(n -> formula(a, b, n))
                .takeWhile(Euler27::isPrime);
        return Math.toIntExact(stream.count());
    }

    private static final Map<Integer, Boolean> m = new HashMap<>();

    public static boolean isPrime(int x) {
        return m.computeIfAbsent(x, n -> {
            if (n > 0) {
                final int limit = (int) Math.sqrt(n);
                for (int check = 2; check <= limit; check++) {
                    if (n % check == 0) {
                        return false;
                    }
                }
                return true;
            } else {
                return false;
            }
        });
    }


    private static int formula(int a, int b, int n) {
        return n * n + a * n + b;
    }
}
