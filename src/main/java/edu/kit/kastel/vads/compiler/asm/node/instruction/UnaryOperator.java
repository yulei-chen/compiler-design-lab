package edu.kit.kastel.vads.compiler.asm.node.instruction;

public enum UnaryOperator {
    NEG,
    NOT;

    @Override
    public String toString() {
        switch (this) {
            case NEG -> {
                return "negl";
            }
            case NOT -> {
                return "notl";
            }
        }
        throw new IllegalStateException();
    }
}
