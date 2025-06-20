package edu.kit.kastel.vads.compiler.asm.node.operand;

public class ImmAsm implements OperandAsm {
    private final int value;

    public ImmAsm(int value) {
        this.value = value;
    }

    public int value() {
        return value;
    }

    @Override
    public String toString() {
        return "$" + value;
    }
}