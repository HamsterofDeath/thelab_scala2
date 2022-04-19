package hod.javacatchup;

import java.math.BigInteger;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Euler63 {
    public static void main(String[] args) {
        var solution = IntStream.iterate(1,e->e+1)
                .map(e->findAll(e).size())
                .takeWhile(e->e>0)
                .sum();
        System.out.println(solution);
    }

    private static List<BigInteger> findAll(int n) {
        return IntStream.iterate(1, e -> e + 1)
                .boxed()
                .map(e -> BigInteger.valueOf(e).pow(n))
                .takeWhile(e->e.toString().length()<=n)
                .filter(e->e.toString().length()==n)
                .toList();
    }
}
