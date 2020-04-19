package hod.training;

public class Ding {
    public static void main(String[] args) {
        var guessed = "";
        guessed += "a";
        guessed += "b";
        guessed += "c";
        guessed += "e";
        guessed += "4";
        guessed += "€";
        guessed += "@";
        guessed += "+";
        System.out.println(guessed);

        System.out.println(guessed.contains("@"));
        System.out.println(guessed.contains("T"));
    }
}