package hod.study;

import java.util.InputMismatchException;
import java.util.Scanner;

public class Main {

    private static final Rezept[] rezepte = new Rezept[3];


    public static String rezeptAnzeigen(int index) {

        if (index > rezepte.length || index <= 0 || rezepte[index - 1] == null) {
            return "Kein Rezept gefunden.";
        }
        return rezepte[index -1].toString();

    }

    public static Rezept rezeptSuchen(String titel) {

        if (titel == null || titel.isBlank()) {
            return null;
        }

        for (Rezept rezept: rezepte) {
            if (rezept != null && rezept.getName().contains(titel)) {
                return rezept;
            }
        }

        return null;
    }

    public static String alleRezepteAuflisten() {

        StringBuilder stringBuilder = new StringBuilder("Rezepte auf der Liste:\n");

        for (int i = 0; i < rezepte.length; i++) {
            if (rezepte[i] != null) {
                stringBuilder.append(String.format("%d. %s\n", i + 1, rezepte[i].getName()));
            }
        }

        return stringBuilder.toString();
    }

    public static boolean rezeptUmrechnen(int index, double faktor) {

        if (index > rezepte.length || index <= 0 || rezepte[index - 1] == null) {
            return false;
        }

        rezepte[index -1].mengenMultiplizieren(faktor);
        return true;

    }

    public static void beispielRezepteLaden() {
        rezepte[0] = new Rezept(
                "Hummus",
                new Zutat[]{
                        new Zutat("Kichererbsen aus der Dose", 300, "g"),
                        new Zutat("Knoblauchzehen", 2, ""),
                        new Zutat("Tahin", 3, "EL"),
                        new Zutat("Olivenöl", 8, "EL"),
                        new Zutat("Kreuzkümmel", 1, "EL")},
                "Die Kichererbsen abgießen, die Knoblauchzehen schälen und den Saft der Zitrone auspressen. Kichererbsen, Tahin, Knoblauch und " +
                "Zitronensaft in einen Mixer geben und pürieren. Nach und nach Öl dazugeben. Nach Belieben mit Kreuzkümmel verfeinern.",
                Kategorie.VORSPEISE,
                6
        )        ;
        rezepte[1] = new Rezept(
                "Penne All'Arrabbiata",
                new Zutat[]{
                        new Zutat("Penne", 400, "g"),
                        new Zutat("Knoblauchzehe", 1, ""),
                        new Zutat("geschälte Tomaten aus der Dose", 380, "g"),
                        new Zutat("getrocknete Chilischoten", 3, ""),
                        new Zutat("Olivenöl", 3, "EL")},
                "Einen Topf mit Wasser für die Pasta aufsetzen. Die Tomaten mit einer Gabel zerdrücken. Die Kerne der Chilischoten entfernen und die Schoten fein hacken. Knoblauchzehe schälen. Olivenöl in eine Pfanne geben, Knoblauch und Chili bei mittlerer Hitze darin schwitzen. Nach ein paar Minuten die Tomaten und ausreichend Salz hinzufügen, einen Deckel auflegen und köcheln lassen. Die Pasta in den Topf geben, sobald das Wasser köchelt. Nach der angegebenen Kochzeit abgießen. Die Knoblauchzehe aus der Sauce entfernen und die Pasta in der Pfanne mit der Sauce vermischen.",
                Kategorie.HAUPTSPEISE,
                4
        )        ;
        rezepte[2] = new Rezept(
                "Nice Cream mit Himbeeren",
                new Zutat[]{
                        new Zutat("Bananen geschält, kleingeschnitten, gefroren", 3, ""),
                        new Zutat("frische Himbeeren", 125, "g"),
                        new Zutat("Ahornsirup", 1, "EL")},
                "Zuerst die Himbeeren und den Ahornsirup in einen Mixer geben und fein pürieren. Dann die Bananen zugeben und auf niedriger Stufe weiter pürieren, bis die gewünschte Konsistenz erreicht ist.",
                Kategorie.DESSERT,
                3
        )        ;
    }

    // Diese Methode dient zum Ausprobieren - wichtig bei der Abgabe ist die Funktionalität der Methoden und Klassen
    public static void main(String[] args) {

        beispielRezepteLaden();

        Scanner scanner = new Scanner(System.in);

        System.out.println("Willkommen in Ihrer Rezeptsammlung, was möchten Sie tun?");

        scanner.useDelimiter("\n");

        int aktion;

        while (true) {

            System.out.println("[1] Rezeptnamen anzeigen\n" +
                               "[2] Ein Rezept anzeigen\n" +
                               "[3] Ein Rezept umrechnen\n" +
                               "[4] Programm beenden");

            try {
                aktion = scanner.nextInt();

                int index;
                double faktor;

                switch (aktion) {
                    case 1:
                        System.out.println(alleRezepteAuflisten());
                        break;

                    case 2:
                        System.out.println("Bitte geben Sie die Nummer des Rezepts ein, das Sie anzeigen möchten:");
                        index = scanner.nextInt();
                        System.out.println(rezeptAnzeigen(index));
                        break;

                    case 3:
                        System.out.println("Bitte geben Sie die Nummer des Rezepts ein, dessen Mengen Sie neu berechnen möchten:");
                        index = scanner.nextInt();
                        System.out.println("Geben Sie nun einen Umrechnungsfaktor ein:");
                        faktor = scanner.nextDouble();
                        if (rezeptUmrechnen(index, faktor)) {
                            System.out.println(String.format("Die Mengen für das Rezept %s wurden mit dem Faktor %.1f multipliziert.", index, faktor));
                            System.out.println(rezeptAnzeigen(index));
                        }
                        else {
                            System.out.println(String.format("Die Mengen für das Rezept %s konnten nicht angepasst werden", index));
                        }
                        break;

                    case 4:
                        scanner.close();
                        System.exit(0);
                    default:
                        System.out.println("Ungültige Eingabe.");

                }
            } catch (InputMismatchException e) {
                // for now: use the input
                scanner.next();
                System.err.println("Ungültige Eingabe.");
            }

        }
    }
}
