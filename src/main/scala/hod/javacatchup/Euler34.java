package hod.javacatchup;

import java.util.Arrays;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Euler34 {
    public static void main(String[] args) {
        // upper bound?
        IntStream.range(3, 1000000).filter(Euler34::isInteresting).forEach(System.out::println);
    }

    private static boolean isInteresting(int n) {
        var sumOfFactorials = String.valueOf(n)
                .chars()
                .map(Character::getNumericValue)
                .map(Euler34::factorial)
                .sum();
        return sumOfFactorials == n;
    }

    private static int factorial(int n) {
        if (n==0) return 1; else return n*factorial(n-1);
    }
}
