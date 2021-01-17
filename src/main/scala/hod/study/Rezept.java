package hod.study;

public class Rezept {

    private final String  name;
    private final Zutat[] zutaten;
    private final String  anleitung;
    private final Kategorie kategorie;
    private       double    portionen;

    public Rezept(final String name, final Zutat[] zutaten, final String anleitung, final Kategorie kategorie, final double portionen) {
        this.name = name;
        this.zutaten = zutaten;
        this.anleitung = anleitung;
        this.kategorie = kategorie;
        this.portionen = portionen;
    }

    public String getName() {
        return name;
    }

    public void mengenMultiplizieren(double faktor) {
        for (int i = 0; i < zutaten.length; i++) {
            final Zutat old = zutaten[i];
            zutaten[i] = new Zutat(old.getName(), old.getMenge() * faktor, old.getEinheit());
        }
        portionen *= faktor;
    }

    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder(name + "\n\n");

        stringBuilder.append(String.format("Kategorie: %s\n", kategorie));

        stringBuilder.append(String.format("Zutaten für %.1f Portionen:\n", portionen));
        for (Zutat z: zutaten) {
            stringBuilder.append(z.toString() + "\n");
        }
        stringBuilder.append("Zubereitung:\n");
        stringBuilder.append(anleitung + "\n");
        return stringBuilder.toString();
    }

}
