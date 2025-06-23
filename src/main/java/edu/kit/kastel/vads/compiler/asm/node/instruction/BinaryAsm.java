package edu.kit.kastel.vads.compiler.asm.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;

public class BinaryAsm implements InstructionAsm {
    private BinaryOperator operator;
    private OperandAsm src;
    private OperandAsm dst;

    public BinaryAsm(BinaryOperator operator, OperandAsm src, OperandAsm dst) {
        this.operator = operator;
        this.src = src;
        this.dst = dst;
    }

    @Override
    public String toString() {
        return operator.toString() + " " + src.toString() + ", " + dst.toString();
    }

    @Override
    public List<OperandAsm> getOperands() {
        return List.of(src, dst);
    }

    @Override
    public void setOperand(int index, OperandAsm operand) {
        if (index == 0) {
            this.src = operand;
        } else if (index == 1) {
            this.dst = operand;
        }
    }
}
