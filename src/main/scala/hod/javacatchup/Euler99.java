package hod.javacatchup;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Euler99 {
    public static void main(String[] args) throws IOException {
        var doubles = Files.lines(Path.of("resource/p99.txt"))
                .map(str -> str.split(Pattern.quote(",")))
                .map(arr -> Math.log10(Integer.parseInt(arr[0])) * Integer.parseInt(arr[1]))
                .collect(Collectors.toList());
        var largest = doubles.stream().max(Double::compareTo).get();
        var index = doubles.indexOf(largest) + 1;
        System.out.println(index);
    }
}
