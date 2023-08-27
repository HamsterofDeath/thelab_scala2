package hod.javacatchup;


import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Euler92 {


    private static Stream<Integer> start(int n) {
        return Stream.iterate(n, Euler92::squareDigits);
    }

    private static int squareDigits(Integer previous) {
        var sum = 0;
        while (previous > 0) {
            int digit = previous % 10;
            sum += digit * digit;
            previous /= 10;
        }
        return sum;
    }

    public static void main(String[] args) {
        var arrivesAt89 = new HashSet<>(List.of(89));
        var arrivesAt1 = new HashSet<>(List.of(1));
        IntStream.range(1, 10000000)
                .forEach(n -> {
                    try {
                        var soFar = new ArrayList<Integer>();
                        start(n).forEach(e -> {
                            soFar.add(e);
                            if (arrivesAt1.contains(e)) {
                                arrivesAt1.addAll(soFar);
                                throw new AbortException();
                            } else if (arrivesAt89.contains(e)) {
                                arrivesAt89.addAll(soFar);
                                throw new AbortException();
                            }
                        });
                    } catch (AbortException ignored) {

                    }
                });
        System.out.println(arrivesAt89.size());
    }

    private static class AbortException extends RuntimeException {
    }
}
