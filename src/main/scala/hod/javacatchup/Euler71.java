package hod.javacatchup;

import hod.EulerUtils;

import java.util.Comparator;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.IntStream;

public class Euler71 {
    record Tuple(int n, int d) {
        double asDouble() {
            return (double) n / d;
        }
    }

    public static void main(String[] args) {
        var reference = 3.0 / 7;
        var limit = 1000000;
        var closest = new AtomicInteger(0);
        var result = IntStream.range(1, limit)
                .boxed()
                .parallel()
                .map(d -> {
                    if (closest.incrementAndGet() % 1000 == 0) {
                        System.out.print(".");
                    }

                    var smallestNumerator = IntStream.range(1, d - 1)
                            .takeWhile(n -> (double) n / d < reference)
                            .filter(n -> EulerUtils.isReducedProperFractionJ(n, d))
                            .boxed()
                            .min((o1, o2) -> {
                                var fractionLeft = reference - (double) o1 / d;
                                var fractionRight = reference - (double) o2 / d;
                                return Double.compare(fractionLeft, fractionRight);
                            })
                            .orElseGet(() -> d - 1);
                    //   System.out.println("Candidate: "+smallestNumerator + "/"+d+"="+
                    //   (smallestNumerator/(double)d));
                    return new Tuple(smallestNumerator, d);
                }).min(Comparator.comparingDouble(o -> Math.abs(reference - o.asDouble())));
        System.out.println(result);

    }
}
