package hod.javacatchup;

import java.awt.*;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;

public class Euler102 {
    public static void main(String[] args) throws IOException {
        var lines = Files.lines(Path.of("resource/euler102.txt"));
        var solution = lines.filter(line-> {
            var coordinates = Arrays.stream(line.split(",")).map(Integer::parseInt).toList();
            var p = new Polygon();
            p.addPoint(coordinates.get(0),coordinates.get(1));
            p.addPoint(coordinates.get(2),coordinates.get(3));
            p.addPoint(coordinates.get(4),coordinates.get(5));
            return p.contains(0,0);
        }).count();
        System.out.println(solution);
    }
}
