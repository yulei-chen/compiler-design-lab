package edu.kit.kastel.vads.compiler.ir_tac.utils;

public class MakeTemp {
    private static int tempCount = 0;

    public static String makeTemp() {
        return "tmp." + tempCount++;
    }
}
