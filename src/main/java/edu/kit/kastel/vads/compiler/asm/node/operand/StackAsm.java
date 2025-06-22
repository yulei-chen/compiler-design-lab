package edu.kit.kastel.vads.compiler.asm.node.operand;

public class StackAsm implements OperandAsm {
    private final int offset;

    public StackAsm(int offset) {
        this.offset = offset;
    }

    @Override
    public String toString() {
        return String.format("-%d(%%rsp)", this.offset);
    }
}
