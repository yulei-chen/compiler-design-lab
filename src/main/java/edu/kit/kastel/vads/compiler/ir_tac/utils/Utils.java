package edu.kit.kastel.vads.compiler.ir_tac.utils;

public class Utils {
    private static int tempCount = 0;
    private static int labelCount = 0;

    public static String makeTemp() {
        return "tmp." + tempCount++;
    }

    public static String makeLabel() {
        return "label." + labelCount++;
    }
}
