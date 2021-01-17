package hod.study;

import java.util.Random;

public class FesttagsRouting {
    public static final int x = 10;
    public static final int y = 10;

    public static class Karte {
        String rahmen;
        boolean befana;
        boolean haus;
        boolean hindernis;

        public Karte(final String rahmen, final boolean befana, final boolean haus, final boolean hindernis) {
            this.rahmen = rahmen;
            this.befana = befana;
            this.haus = haus;
            this.hindernis = hindernis;
        }
    }

    public static Karte[][] reisekarte = new Karte[x][y];

    public static void testPrint() {
        for (int y = 0 ;y<FesttagsRouting.y;y++) {
            for (int x = 0 ;x<FesttagsRouting.x;x++) {
                String zeichen;
                final Karte zelle = reisekarte[x][y];
                if (zelle == null) {
                    zeichen = " ";
                } else if (zelle.befana) {
                    zeichen = "B";
                } else if (zelle.haus) {
                    zeichen = "H";
                } else if (zelle.hindernis) {
                    zeichen = "O";
                } else {
                    zeichen = zelle.rahmen;
                }
                System.out.print(zeichen);
            }
            System.out.println();
        }
    }

    public static void erzeugeRahmen() {
        for (int x1 = 0;x1<x;x1++) {
            final Karte minus = new Karte("-", false, false, false);
            reisekarte[x1][0] = minus;
            reisekarte[x1][y-1] = minus;
        }
        for (int y1 = 0;y1<y;y1++) {
            final Karte bar = new Karte("|", false, false, false);
            reisekarte[0][y1] = bar;
            reisekarte[x-1][y1] = bar;
        }

        final Karte ecke = new Karte("+", false, false, false);
        reisekarte[0][0] = ecke;
        reisekarte[x-1][0] = ecke;
        reisekarte[0][y-1] = ecke;
        reisekarte[x-1][y-1] = ecke;
    }

    public static void erzeugeObjekte(int anzahlHaeuser, int anzahlHindernisse) {
        reisekarte[1][1] = new Karte("", true, false, false);
        Random r = new Random();
        if (anzahlHaeuser + anzahlHindernisse + 1 > (x - 1) * (y - 1)) {
            throw new RuntimeException("Zu wenig freie Felder");
        }
        while (anzahlHaeuser > 0) {
            int x = r.nextInt(FesttagsRouting.x - 2) + 1;
            int y = r.nextInt(FesttagsRouting.y - 2) + 1;
            if (reisekarte[x][y] == null) {
                reisekarte[x][y] = new Karte("", false, true, false);
                anzahlHaeuser--;
            }
        }
        while (anzahlHindernisse > 0) {
            int x = r.nextInt(FesttagsRouting.x - 2) + 1;
            int y = r.nextInt(FesttagsRouting.y - 2) + 1;
            if (reisekarte[x][y] == null) {
                reisekarte[x][y] = new Karte("", false, false, true);
                anzahlHindernisse--;
            }
        }
    }

    public static void main(String[] args) {
        erzeugeRahmen();
        erzeugeObjekte(7,7);
        testPrint();
    }

}
