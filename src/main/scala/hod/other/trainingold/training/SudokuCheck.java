package hod.other.trainingold.training;

public class SudokuCheck {

    public static void main(String[] args) {
        int[] sudoku3x3 = new int[]{
                4, 2, 6, 5, 7, 1, 3, 9, 8,
                8, 5, 7, 2, 9, 3, 1, 4, 6,
                1, 3, 9, 4, 6, 8, 2, 7, 5,
                9, 7, 1, 3, 8, 5, 6, 2, 4,
                5, 4, 3, 7, 2, 6, 8, 1, 9,
                6, 8, 2, 1, 4, 9, 7, 5, 3,
                7, 9, 4, 6, 3, 2, 5, 8, 1,
                2, 6, 5, 8, 1, 4, 9, 3, 7,
                3, 1, 8, 9, 5, 7, 4, 6, 2
        };
        System.out.println(isValid(sudoku3x3));
    }

    private static boolean isValid(int[] entireGrid) {
        for (int i = 0; i < 9; i++) {
            boolean ok = isBlockCorrect(extract3x3(i, entireGrid)) &&
                         isBlockCorrect(extractColumn(i, entireGrid)) &&
                         isBlockCorrect(extractRow(i, entireGrid));
            if (!ok) return false;
        }
        return true;
    }

    private static int[] extract3x3(int blockIndex, int[] entireGrid) {
        final int xStart = (blockIndex % 3) * 3;
        final int yStart = (blockIndex / 3) * 3;

        int collected = 0;
        int[] relevantNumbers = new int[9];
        for (int x = xStart; x < xStart + 3; x++) {
            for (int y = yStart; y < yStart + 3; y++) {
                relevantNumbers[collected++] = entireGrid[x + y * 9];
            }
        }
        return relevantNumbers;
    }

    private static int[] extractRow(int rowIndex, int[] entireGrid) {
        int[] relevantNumbers = new int[9];
        for (int i = 0; i < 9; i++) {
            relevantNumbers[i] = entireGrid[i + rowIndex * 9];
        }
        return relevantNumbers;
    }

    private static int[] extractColumn(int colIndex, int[] entireGrid) {
        int[] relevantNumbers = new int[9];
        for (int i = 0; i < 9; i++) {
            relevantNumbers[i] = entireGrid[i * 9 + colIndex];
        }
        return relevantNumbers;
    }

    private static boolean isBlockCorrect(int[] nine) {
        boolean[] numbersFound = new boolean[9];
        for (int cell : nine) {
            if (cell == 0) return false;
            numbersFound[cell - 1] = true;
        }
        for (boolean b : numbersFound) {
            if (!b) return false;
        }
        return true;
    }


}
