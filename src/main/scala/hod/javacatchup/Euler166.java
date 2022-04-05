package hod.javacatchup;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.stream.IntStream;

public class Euler166 {

    public static int geradeZahlen(int von, int bis) {
        int count = 0;
        for (int i = von; i < bis; i++) {
            final boolean istGerade = i % 2 == 0;
            if (istGerade) {
                System.out.println(i + " ist gerade");
                count++;
            }
        }
        return count;
    }

    public static int biggest(int[] arr, int idx, int biggest) {
        return idx==arr.length?Integer.MIN_VALUE:Math.max(arr[idx], biggest(arr,idx+1,123456));
    }


    public static int summe(int[] zahlen) {
        int summe = 0;
        for (int i = 0; i < zahlen.length; i++) {
            // ???
        }
        return summe;
    }

    public static int whatIMeant() {
        return IntStream.range(0, 37).map(Euler166::countBruteForce).sum();
    }

    public static int countBruteForce(int sum) {
        final int empty = -1000;
        int c1_1, c2_1, c3_1, c4_1, c1_2, c2_2, c3_2, c4_2, c1_3, c2_3, c3_3, c4_3, c1_4, c2_4, c3_4, c4_4 = empty;
        final int maxDigit = 10;
        final int minDigit = 0;
        int counted = 0;
        for (int i1 = minDigit; i1 < maxDigit; i1++) {
            c1_1 = i1;
            for (int i2 = minDigit; i2 < maxDigit; i2++) {
                c2_1 = i2;
                for (int i3 = minDigit; i3 < maxDigit; i3++) {
                    c3_1 = i3;
                    for (int i4 = minDigit; i4 < maxDigit; i4++) {
                        c4_1 = i4;
                        if (check4(sum, c1_1, c2_1, c3_1, c4_1)) {
                            for (int i5 = minDigit; i5 < maxDigit; i5++) {
                                c1_2 = i5;
                                for (int i6 = minDigit; i6 < maxDigit; i6++) {
                                    c2_2 = i6;
                                    for (int i7 = minDigit; i7 < maxDigit; i7++) {
                                        c3_2 = i7;
                                        for (int i8 = minDigit; i8 < maxDigit; i8++) {
                                            c4_2 = i8;
                                            if (check4(sum, c1_2, c2_2, c3_2, c4_2)) {
                                                for (int i9 = minDigit; i9 < maxDigit; i9++) {
                                                    c1_3 = i9;
                                                    for (int i10 = minDigit; i10 < maxDigit; i10++) {
                                                        c2_3 = i10;
                                                        for (int i11 = minDigit; i11 < maxDigit; i11++) {
                                                            c3_3 = i11;
                                                            for (int i12 = minDigit; i12 < maxDigit; i12++) {
                                                                c4_3 = i12;
                                                                if (check4(sum, c1_3, c2_3, c3_3, c4_3)) {
                                                                    for (int i13 = minDigit; i13 < maxDigit; i13++) {
                                                                        c1_4 = i13;
                                                                        if (check4(sum, c1_1, c1_2, c1_3, c1_4) &&
                                                                            check4(sum, c4_1, c3_2, c2_3, c1_4)) {
                                                                            for (int i14 = minDigit; i14 < maxDigit; i14++) {
                                                                                c2_4 = i14;
                                                                                if (check4(sum, c2_1, c2_2, c2_3, c2_4)) {
                                                                                    for (int i15 = minDigit; i15 < maxDigit; i15++) {
                                                                                        c3_4 = i15;
                                                                                        if (check4(sum, c3_1, c3_2, c3_3, c3_4)) {
                                                                                            for (int i16 = minDigit; i16 < maxDigit; i16++) {
                                                                                                c4_4 = i16;
                                                                                                if (check4(sum, c4_1, c4_2, c4_3, c4_4) &&
                                                                                                    check4(sum, c1_1, c2_2, c3_3, c4_4)) {
                                                                                                    //                                                                                                    var debugLines = c1_1+ " "+c2_1+" "+c3_1+" "+c4_1+"\n"+
                                                                                                    //                                                                                                                     c1_2+ " "+c2_2+" "+c3_2+" "+c4_2+"\n"+
                                                                                                    //                                                                                                                     c1_3+ " "+c2_3+" "+c3_3+" "+c4_3+"\n"+
                                                                                                    //                                                                                                                     c1_4+ " "+c2_4+" "+c3_4+" "+c4_4+"\n";
                                                                                                    //                                                                                                    System.out.println(debugLines);
                                                                                                    counted++;
                                                                                                }
                                                                                                c4_4 = empty;
                                                                                            }
                                                                                        }
                                                                                        c3_4 = empty;
                                                                                    }
                                                                                }
                                                                                c2_4 = empty;
                                                                            }
                                                                        }
                                                                        c1_4 = empty;
                                                                    }
                                                                }
                                                                c4_3 = empty;
                                                            }
                                                            c3_3 = empty;
                                                        }
                                                        c2_3 = empty;
                                                    }
                                                    c1_3 = empty;
                                                }
                                            }
                                            c4_2 = empty;
                                        }
                                        c3_2 = empty;
                                    }
                                    c2_2 = empty;
                                }
                                c1_2 = empty;
                            }
                        }
                        c4_1 = empty;
                    }
                    c3_1 = empty;
                }
                c2_1 = empty;
            }
            c1_1 = empty;
        }
        System.out.println("Solution for " + sum + " = " + counted);
        return counted;
    }

    public static boolean check4(int sum, int v1, int v2, int v3, int v4) {
        int sumOfValues = v1+v2+v3+v4;
        return sumOfValues == sum;
    }

    public class MyLines {
        int           sum;
        List<Integer> list = new ArrayList<>();

        public MyLines(int sum, int a, int b, int c, int d) {
            this.sum = sum;
            list.add(a);
            list.add(b);
            list.add(c);
            list.add(d);
        }

        public int getSum() {
            return sum;
        }

        public List<Integer> getList() {
            return list;
        }
    }

    private List<int[]> createListOfTwo() {
        List<int[]> allPossibleTwo = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 10; j++) {
                int[] part = new int[2];
                part[0] = i;
                part[1] = j;
                allPossibleTwo.add(part);
            }
        }
        return allPossibleTwo;
    }

    private List<MyLines> createMapOfFour() {
        List<int[]> listOfTwo = createListOfTwo();
        List<MyLines> listOfFour = new ArrayList<>();
        for (int[] a : listOfTwo) {
            for (int[] b : listOfTwo) {
                MyLines line = new MyLines(a[0] + a[1] + b[0] + b[1], a[0], a[1], b[0], b[1]);
                listOfFour.add(line);
            }
        }
        return listOfFour;
    }

    public int howManyEqualMatrices() {
        List<MyLines> myDiagonal = createMapOfFour();
        int counter = 0;
        for (int i = 0; i < 37; i++) {
            for (MyLines a : myDiagonal) {
                if (a.sum == i) {
                    for (MyLines b : myDiagonal) {
                        if (b.sum == i) {
                            if (isMatrixValid(i, a, b)) {
                                int oldCounter = counter;
                                counter = counter + checkAllPossibilities(i, a, b);
                                if (i == 2) {
                                    System.out.println("for " + i + " we have " + (counter - oldCounter));
                                    System.out.println(a.getList() + " as well as " + b.getList());
                                }
                            }
                        }
                    }
                }
            }
        }
        return counter;
    }

    private int checkAllPossibilities(int sum, MyLines a, MyLines b) {
        int firstCounter = 0;
        int secondCounter = 0;

        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 10; j++) {
                if (a.getList().get(0) + i + j + b.getList().get(3) == sum) {
                    int rest1 = sum - (i + a.getList().get(1) + b.getList().get(1));
                    if (rest1 < 10 && rest1 > (-1)) {
                        int rest2 = sum - (j + b.getList().get(2) + a.getList().get(2));
                        if (rest2 < 10 && rest2 > (-1)) {
                            if (rest1 + rest2 + a.getList().get(3) + b.getList().get(0) == sum) {
                                firstCounter++;
                            }
                        }
                    }
                }
            }
        }

        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 10; j++) {
                if (a.getList().get(0) + i + j + b.getList().get(0) == sum) {
                    int rest1 = sum - (i + a.getList().get(1) + b.getList().get(2));
                    if (rest1 < 10 && rest1 > (-1)) {
                        int rest2 = sum - (j + b.getList().get(1) + a.getList().get(2));
                        if (rest2 < 10 && rest2 > (-1)) {
                            if (rest1 + rest2 + a.getList().get(0) + b.getList().get(0) == sum) {
                                secondCounter++;
                            }
                        }
                    }
                }
            }
        }
        return firstCounter * secondCounter;
    }

    private boolean isMatrixValid(int sum, MyLines a, MyLines b) {
        return (a.getList().get(0) + b.getList().get(0) <= sum && b.getList().get(0) + a.getList().get(3) <= sum &&
                a.getList().get(3) + b.getList().get(3) <= sum && b.getList().get(3) + a.getList().get(0) <= sum &&
                a.getList().get(1) + b.getList().get(1) <= sum && a.getList().get(2) + b.getList().get(2) <= sum &&
                a.getList().get(1) + b.getList().get(2) <= sum && a.getList().get(2) + b.getList().get(1) <= sum);
    }



    public static void main(String[] args) {
      //  System.out.println(biggest(new int[]{1,2,7,5,4},0, -1234));
        //System.out.println(geradeZahlen(1, 1000));

        System.out.println(whatIMeant());
        //        Euler166 sol = new Euler166();
        //        System.out.println(sol.howManyEqualMatrices());
    }

}