package hod.study;

import java.text.DecimalFormat;

public class Zutat {

    private final String name;
    private final double menge;
    private final String einheit;

    public Zutat(final String name, final double menge, final String einheit) {
        this.name = name;
        this.menge = menge;
        this.einheit = einheit;
    }

    @Override
    public String toString() {
        var df = new DecimalFormat("#0.00");
        return df.format(menge) + " "+einheit+" "+name;
    }

    public String getName() {
        return name;
    }

    public double getMenge() {
        return menge;
    }

    public String getEinheit() {
        return einheit;
    }
}