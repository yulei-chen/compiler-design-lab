package edu.kit.kastel.vads.compiler.asm.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;

public class CmpAsm implements InstructionAsm {
    private OperandAsm src1;
    private OperandAsm src2;

    public CmpAsm(OperandAsm src1, OperandAsm src2) {
        this.src1 = src1;
        this.src2 = src2;
    }

    @Override
    public List<OperandAsm> getOperands() {
        return List.of(src1, src2);
    }

    @Override
    public void setOperand(int index, OperandAsm operand) {
        if (index == 0) {
            this.src1 = operand;
        } else if (index == 1) {
            this.src2 = operand;
        } else {
            throw new IllegalArgumentException("Invalid operand index: " + index);
        }
    }

    @Override
    public String toString() {
        return "cmp " + src1.toString() + ", " + src2.toString();
    }   
}
