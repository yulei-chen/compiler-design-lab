package edu.kit.kastel.vads.compiler.asm.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;

public class JmpCCAsm implements InstructionAsm {
    private final CondCode condCode;
    private final String target;

    public JmpCCAsm(CondCode condCode, String target) {
        this.condCode = condCode;
        this.target = target;
    }

    @Override
    public List<OperandAsm> getOperands() {
        return List.of();
    }

    @Override
    public void setOperand(int index, OperandAsm operand) {
        throw new UnsupportedOperationException("JmpCC does not have operands");
    }

    @Override
    public String toString() {
        return "j" + condCode.toString() + " ." + target;
    }
}
