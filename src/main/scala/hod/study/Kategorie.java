package hod.study;

public enum Kategorie {

    VORSPEISE, HAUPTSPEISE, DESSERT;

    @Override
    public String toString() {
        switch (this) {
            case VORSPEISE:
                return "Vorspeise";
            case HAUPTSPEISE:
                return "Hauptspeise";
            case DESSERT:
                return "Dessert";
        }
        return "?";
    }
}
