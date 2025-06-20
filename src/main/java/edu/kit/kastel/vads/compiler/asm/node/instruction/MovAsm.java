package edu.kit.kastel.vads.compiler.asm.node.instruction;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;

public class MovAsm implements InstructionAsm {
    private final OperandAsm src;
    private final OperandAsm dst;

    public MovAsm(OperandAsm src, OperandAsm dst) {
        this.src = src;
        this.dst = dst;
    }

    public OperandAsm src() {
        return src;
    }

    public OperandAsm dst() {
        return dst;
    }

    @Override
    public String toString() {
        return "movl " + src.toString() + ", " + dst.toString();
    }
}
