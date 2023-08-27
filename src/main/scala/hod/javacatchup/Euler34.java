package hod.javacatchup;

import java.util.stream.IntStream;

public class Euler34 {
    public static void main(String[] args) {
        var upperBound = 7 * factorial(9);
        IntStream.range(3, upperBound).filter(Euler34::isInteresting).forEach(System.out::println);
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
        if (n == 0) {return 1;} else return n * factorial(n - 1);
    }
}
