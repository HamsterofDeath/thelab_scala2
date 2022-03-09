package hod.other.trainingold.training;

public class RandomStuff {

    public static int countWords(String str) {
        var words = str.split(" ");
        return words.length;
    }

    public static void printLengths(String str) {
        String currentWord = "";
        for (int i = 0; i < str.length(); i++) {
            var now = str.charAt(i);
            if (now != ' ') {
                currentWord = currentWord + now;
            } else {
                System.out.println("Found: " + currentWord + ", " + currentWord.length() + " chars");
                currentWord = "";
            }
        }
        System.out.println("Found: " + currentWord + ", " + currentWord.length() + " chars");
    }

    public static boolean isPrime(int x) {
        final int limit = (int) Math.sqrt(x);
        for (int check = 2; check <= limit; check++) {
            if (isDivisibleBy(x, check)) {
                return false;
            }
        }
        return true;
    }

    public static void main(String[] args) {
        //        for (int i = 2; i < 10000;i++) {
        //            if (isPrime(i)) {
        //                System.out.println(i);
        //            }
        //        }

        String blah = "this is a sentence with words";
        printLengths(blah);

    }

    public static boolean isDivisibleBy(int checkMe, int by) {
        return checkMe % by == 0;
    }
}
