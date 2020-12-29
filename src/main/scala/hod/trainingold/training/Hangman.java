package hod.trainingold.training;

import java.util.HashSet;
import java.util.Scanner;
import java.util.Set;


public class Hangman {

    public static String printTheThing(String word, Set<Character> guessed) {
        StringBuilder progress = new StringBuilder();
        for (int i = 0; i < word.length(); i++) {
            if (guessed.contains(word.charAt(i))) {
                progress.append(word.charAt(i));
            } else {
                progress.append("-");
            }
        }
        return progress.toString();
    }

    public static void main(String[] args) {
        var correct = "password";
        Set<Character> guessed = new HashSet<>();
        var tries = 0;
        while (!printTheThing(correct, guessed).equals(correct)) {
            System.out.println("Current state: " + printTheThing(correct, guessed));
            System.out.println("Guess a letter:");
            var userGuess = readLineFromUser();
            if (userGuess.length() == 1) {
                var myChar = userGuess.charAt(0);
                guessed.add(myChar);
                tries += 1;
            } else {
                System.out.println("Wrong input");
            }
        }
        System.out.println("You took " + tries + " tries to guess all the letters");
    }

    private static String readLineFromUser() {
        Scanner in = new Scanner(System.in);
        return in.nextLine();
    }
}
