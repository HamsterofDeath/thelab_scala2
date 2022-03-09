package hod.other.trainingold.training;

import java.util.ArrayList;
import java.util.List;

public class Euler89 {

    public static List<Integer> romanToNumber(String roman) {
        List<Integer> numbers = new ArrayList<Integer>();

        for (int i = 0; i < roman.length(); i++) {
            char c = roman.charAt(i);
            if (c == 'M') {
                numbers.add(1000);
            } else if (c == 'D') {
                numbers.add(500);
            } else if (c == 'C') {
                numbers.add(100);
            } else if (c == 'L') {
                numbers.add(50);
            } else if (c == 'X') {
                numbers.add(10);
            } else if (c == 'V') {
                numbers.add(5);
            } else if (c == 'I') {
                numbers.add(1);
            } else {
                throw new RuntimeException("???");
            }
        }
        return numbers;
    }

    public static int calculateNumber(List<Integer> numbers) {
        int sum = 0;

        for (int i = 0; i < numbers.size() - 1; i++) {
            int current = numbers.get(i);
            if (current >= numbers.get(i + 1)) {
                sum += current;
            } else {
                sum -= current;
            }
        }
        sum += numbers.get(numbers.size() - 1);
        return sum;
    }

    public static String numberToRoman(int sum) {
        String romanShort = "";
        int romanShortToNumber = 0;
        while (sum - romanShortToNumber != 0) {
            var remaining = sum - romanShortToNumber;
            if (remaining >= 1000) {
                romanShort += "M";
            } else if (remaining>=900) {
                romanShort += "DM";
            } else if (remaining >= 500) {
                romanShort += "D";
            } else if (remaining >= 400) {
                romanShort += "CD";
            } else if (remaining >= 100) {
                romanShort += "C";
            } else if (remaining >= 90) {
                romanShort += "LC";
            } else if (remaining >= 50) {
                romanShort += "L";
            } else if (remaining >= 40) {
                romanShort += "XL";
            } else if (remaining >= 10) {
                romanShort += "X";
            } else if (remaining >= 9) {
                romanShort += "IX";
            } else if (remaining >= 5) {
                romanShort += "V";
            } else if (remaining >= 4) {
                romanShort += "IV";
            } else if (remaining >= 1) {
                romanShort += "I";
            }
            System.out.println("Roman short so far: "+romanShort);
            romanShortToNumber = calculateNumber(romanToNumber(romanShort));
            System.out.println("Value: "+romanShortToNumber);
        }
        return romanShort;
    }


    public static void main(String[] args) {

        System.out.println(romanToNumber("MMMMDCLXXII"));
        System.out.println(calculateNumber(romanToNumber("MMMMDCLXXII")));
        System.out.println(numberToRoman(calculateNumber(romanToNumber("MMMMDCLXXII"))));
    }
}