package edu.kit.kastel.vads.compiler.asm.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;

public class FunctionAsm implements InstructionAsm {
    private final String name;

    public FunctionAsm(String name) {
        this.name = name;
    }

    @Override
    public List<OperandAsm> getOperands() {
        return List.of();
    }

    @Override
    public void setOperand(int index, OperandAsm operand) {
        throw new UnsupportedOperationException("Function does not have operands");
    }

    @Override
    public String toString() {
        return ".global " + name + "\n" + 
        name + ":\n" +
        "pushq %rbp\n" +
        "movq %rsp, %rbp";
    }
}
