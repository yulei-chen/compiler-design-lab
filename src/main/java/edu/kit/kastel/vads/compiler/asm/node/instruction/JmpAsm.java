package edu.kit.kastel.vads.compiler.asm.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;

public class JmpAsm implements InstructionAsm {
    private final String target;

    public JmpAsm(String target) {
        this.target = target;
    }

    @Override
    public List<OperandAsm> getOperands() {
        return List.of();
    }

    @Override
    public void setOperand(int index, OperandAsm operand) {
        throw new UnsupportedOperationException("Jmp does not have operands");
    }

    @Override
    public String toString() {
        return "jmp ." + target;
    }
}
