package edu.kit.kastel.vads.compiler.asm.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;

public class CallAsm implements InstructionAsm {
    private final String functionName;

    public CallAsm(String functionName) {
        this.functionName = functionName;
    }

    @Override
    public List<OperandAsm> getOperands() {
        return List.of();
    }

    @Override
    public void setOperand(int index, OperandAsm operand) {
        throw new UnsupportedOperationException("Call does not have operands");
    }

    @Override
    public String toString() {
        return "call " + this.functionName;
    }
}
