package edu.kit.kastel.vads.compiler.asm.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;

public class IdivAsm implements InstructionAsm {
    private OperandAsm operand;

    public IdivAsm(OperandAsm operand) {
        this.operand = operand;
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

    @Override
    public String toString() {
        return "idivl " + operand.toString();
    }
}
