package edu.kit.kastel.vads.compiler.asm.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;
import edu.kit.kastel.vads.compiler.asm.node.operand.RegAsm;

public class Push implements InstructionAsm {
    private final OperandAsm operand;

    public Push(OperandAsm operand) {
        this.operand = operand;
    }

    @Override
    public List<OperandAsm> getOperands() {
        return List.of(this.operand);
    }

    @Override
    public void setOperand(int index, OperandAsm operand) {
        throw new UnsupportedOperationException("Push does not have operands");
    }

    @Override
    public String toString() {
        return "pushq " + ((RegAsm) this.operand).to8ByteString();
    }
}
