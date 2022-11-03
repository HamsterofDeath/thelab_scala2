package hod.javacatchup;

public class Euler145 {

    private static long reversedDigits(int n) {
        long result = 0L;
        while (n > 0) {
            result *= 10;
            result += n % 10;
            n /= 10;
        }
        return result;
    }

    private static boolean isReversible(int n) {
        return n % 10 != 0 && allDigitsAreOdd(n + reversedDigits(n));
    }

    private static boolean allDigitsAreOdd(long n) {
        while (n > 0) {
            if (n % 2 == 0) return false;
            n /= 10;
        }
        return true;
    }

    public static void main(String[] args) {
        int found = 0;
        for (int i = 0; i < 1_000_000_000; i++) {
            if (isReversible(i)) {
                found += 1;
            }
        }
        System.out.println(found);
    }
}
