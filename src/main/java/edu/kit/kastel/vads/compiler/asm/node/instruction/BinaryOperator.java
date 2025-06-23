package edu.kit.kastel.vads.compiler.asm.node.instruction;

public enum BinaryOperator {
    ADD,
    SUB,
    MUL,
    ;

    @Override
    public String toString() {
        switch (this) {
            case ADD -> {
                return "addl";
            }
            case SUB -> {
                return "subl";
            }
            case MUL -> {
                return "imull";
            }
            default -> {
                throw new IllegalStateException("Unknown binary operator: " + this);
            }
        }
    }
}
