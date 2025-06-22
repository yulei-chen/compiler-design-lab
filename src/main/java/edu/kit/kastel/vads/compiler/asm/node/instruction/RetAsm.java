package edu.kit.kastel.vads.compiler.asm.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;

public class RetAsm implements InstructionAsm {
    public RetAsm() {
    }

    @Override
    public String toString() {
        return "ret";
    }

    @Override
    public List<OperandAsm> getOperands() {
        return List.of();
    }

    @Override
    public void setOperand(int index, OperandAsm operand) {
        // do nothing
    }
}