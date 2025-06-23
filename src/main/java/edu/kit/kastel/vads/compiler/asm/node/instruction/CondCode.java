package edu.kit.kastel.vads.compiler.asm.node.instruction;

public enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
    ;

    @Override
    public String toString() {
        return switch (this) {
            case E -> "e";
            case NE -> "ne";
            case G -> "g";
            case GE -> "ge";
            case L -> "l";
            case LE -> "le";
        };
    }
}
