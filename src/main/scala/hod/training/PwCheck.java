package hod.training;

public class PwCheck {

    private static boolean isValid(String pw) {
        // min 6 zeichen
        // max 24 zeichen
        // mind 1 kleiner buchstabe
        // mind 1 großer buchstabe
        // mind 1 ziffer buchstabe
        // mind 1 zeichen das weder buchstabe noch ziffer ist
        // keine leerzeichen an anfang und ende
        return false;
    }

    public static void main(String[] args) {
        test("hallo", false);
        test("123456", false);
        test("xxxAa3§xxx", true);
    }

    private static void test(String pw, boolean expectedValid) {
        var status = isValid(pw) == expectedValid ? "ok" : "nicht ok";
        System.out.println("test für passwort " + pw + " ist " + status);
    }
}
