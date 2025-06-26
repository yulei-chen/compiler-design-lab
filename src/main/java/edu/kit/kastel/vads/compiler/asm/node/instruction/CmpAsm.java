package edu.kit.kastel.vads.compiler.asm.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.operand.ImmAsm;
import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;
import edu.kit.kastel.vads.compiler.asm.node.operand.RegAsm;
import edu.kit.kastel.vads.compiler.asm.node.operand.RegType;
import edu.kit.kastel.vads.compiler.asm.node.operand.StackAsm;

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
        // NOTE: operands cannot be stack at the same time
        if (src1 instanceof StackAsm && src2 instanceof StackAsm) {
            return "movl " + src1.toString() + ", " + new RegAsm(RegType.CX).toString() + "\n" +
                   "cmpl " + new RegAsm(RegType.CX).toString() + ", " + src2.toString();
        // NOTE: src2 cannot be immediate
        } else if (src2 instanceof ImmAsm) {
            return "movl " + src2.toString() + ", " + new RegAsm(RegType.CX).toString() + "\n" +
                   "cmpl " + src1.toString() + ", " + new RegAsm(RegType.CX).toString();
        } else {
            return "cmpl " + src1.toString() + ", " + src2.toString();
        }
    }   
}
