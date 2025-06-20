package edu.kit.kastel.vads.compiler.asm.node.instruction;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;

public class UnaryAsm implements InstructionAsm {
    private final UnaryOperator operator;
    private final OperandAsm operand;

    public UnaryAsm(UnaryOperator operator, OperandAsm operand) {
        this.operator = operator;
        this.operand = operand;
    }

    public OperandAsm operand() {
        return operand;
    }

    public UnaryOperator operator() {
        return operator;
    }

    @Override
    public String toString() {
        return operator.toString() + " " + operand.toString();
    }
}
