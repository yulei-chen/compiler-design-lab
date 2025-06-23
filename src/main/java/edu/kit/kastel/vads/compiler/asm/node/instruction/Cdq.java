package edu.kit.kastel.vads.compiler.asm.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;

public class Cdq implements InstructionAsm {
    @Override
    public String toString() {
        return "cdq";
    }

    @Override
    public List<OperandAsm> getOperands() {
        return List.of();
    }

    @Override
    public void setOperand(int index, OperandAsm operand) {
        throw new UnsupportedOperationException("Cdq does not have operands");
    }
}
