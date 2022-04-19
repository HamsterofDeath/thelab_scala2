package hod.javacatchup;

import hod.EulerUtils;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.IntStream;

public class Euler71 {
    public static void main(String[] args) {
        var reference = 3.0 / 7;
        var limit = 1000000;
        var closest = new AtomicInteger(0);
        var result = IntStream.range(1, limit)
                .boxed()
                .parallel()
                .flatMap(d -> {
                    if(closest.incrementAndGet()%1000==0) {
                        System.out.print(".");
                    }

                    return IntStream.range(1, d - 1)
                            .takeWhile(n -> (double) n / d < reference)
                            .filter(n->EulerUtils.isReducedProperFractionJ(n,d))
                            .mapToDouble(n -> reference - (double) n / d)
                            .min()
                            .stream()
                            .boxed();
                }).min(Double::compareTo);
        System.out.println(result);

    }
}
