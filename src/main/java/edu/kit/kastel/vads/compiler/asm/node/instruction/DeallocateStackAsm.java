package edu.kit.kastel.vads.compiler.asm.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;

public class DeallocateStackAsm implements InstructionAsm {
    private final int stackOffset;

    public DeallocateStackAsm(int stackOffset) {
        this.stackOffset = stackOffset;
    }
    
    @Override
    public List<OperandAsm> getOperands() {
        return List.of();
    }

    @Override
    public void setOperand(int index, OperandAsm operand) {
        throw new UnsupportedOperationException("DeallocateStack does not have operands");
    }

    @Override
    public String toString() {
        return "addq $" + stackOffset + ", %rsp";
    }
}
