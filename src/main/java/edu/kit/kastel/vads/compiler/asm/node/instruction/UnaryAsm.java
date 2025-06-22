package edu.kit.kastel.vads.compiler.asm.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;

public class UnaryAsm implements InstructionAsm {
    private final UnaryOperator operator;
    private OperandAsm operand;

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

    @Override
    public List<OperandAsm> getOperands() {
        return List.of(operand);
    }

    @Override
    public void setOperand(int index, OperandAsm operand) {
        if (index == 0) {
            this.operand = operand;
        }
    }
}
