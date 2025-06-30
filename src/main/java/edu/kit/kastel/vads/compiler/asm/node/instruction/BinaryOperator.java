package edu.kit.kastel.vads.compiler.asm.node.instruction;

public enum BinaryOperator {
    ADD,
    SUB,
    MUL,
    BIT_AND,
    BIT_OR,
    BIT_XOR,
    SHIFT_LEFT,
    SHIFT_RIGHT,
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
            case BIT_AND -> {
                return "andl";
            }
            case BIT_OR -> {
                return "orl";
            }
            case BIT_XOR -> {
                return "xorl";
            }
            case SHIFT_LEFT -> {
                return "sall";
            }
            case SHIFT_RIGHT -> {
                return "sarl";
            }
            default -> {
                throw new IllegalStateException("Unknown binary operator: " + this);
            }
        }
    }
}
