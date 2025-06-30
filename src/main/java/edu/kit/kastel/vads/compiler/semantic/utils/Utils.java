package edu.kit.kastel.vads.compiler.semantic.utils;

public class Utils {
    private static int identCount = 0;

    public static String makeVariable(String variableName) {
        return variableName + "." + identCount++;
    }
}