package edu.kit.kastel.vads.compiler.asm.node.operand;

public enum RegType {
    AX,
    R10;

    @Override
    public String toString() {
        switch (this) {
            case AX -> {
                return "%eax";
            }
            case R10 -> {
                return "%r10d";
            }
        }
        throw new IllegalStateException();
    }
}
