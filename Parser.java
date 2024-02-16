import java.net.*;
import java.io.*;

public class Parser {
    public static void main(String[] args) throws Exception {
        if (args.length < 1) {
            System.err.println("Need host!");
            System.exit(1);
        }

        final var socket = new Socket(args[0], 6969);
        final var in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        final var out = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));

        System.out.println("Bridge connected");

        in.lines().forEach(line -> {
            try {
                System.out.printf("Request: %s%n", line);
                final var proc = new ProcessBuilder("_build/install/default/bin/ocaml_gram").start();

                final var procOut = proc.getOutputStream();
                procOut.write(line.getBytes());
                procOut.close();

                final var resultBytes = proc.getInputStream().readAllBytes();
                final var sanitized = new String(resultBytes).replaceAll("\n", "\0");

                System.out.printf("Parsed: %s%n", sanitized);

                out.write(sanitized);
                out.newLine();
                out.flush();
            } catch (Exception e) {
            }
        });
    }
}
