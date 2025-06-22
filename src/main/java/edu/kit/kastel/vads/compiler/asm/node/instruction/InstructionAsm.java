package edu.kit.kastel.vads.compiler.asm.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;

public interface InstructionAsm {
    // For register allocation to replace pseudo register with real register
    public List<OperandAsm> getOperands();
    public void setOperand(int index, OperandAsm operand);
}
