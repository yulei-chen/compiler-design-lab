package edu.kit.kastel.vads.compiler.asm.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;
import edu.kit.kastel.vads.compiler.asm.node.operand.RegAsm;

public class SetCCAsm implements InstructionAsm {
    private final CondCode condCode;
    private OperandAsm dst;

    public SetCCAsm(CondCode condCode, OperandAsm dst) {
        this.condCode = condCode;
        this.dst = dst;
    }

    @Override
    public List<OperandAsm> getOperands() {
        return List.of(dst);
    }

    @Override
    public void setOperand(int index, OperandAsm operand) {
        if (index == 0) {
            this.dst = operand;
        } else {
            throw new IllegalArgumentException("Invalid operand index: " + index);
        }
    }

    @Override
    public String toString() {
        return "set" + condCode.toString() + " " + (dst instanceof RegAsm ? ((RegAsm) dst).toOneByteString() : dst.toString());
    }
}
