package edu.kit.kastel.vads.compiler.asm.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;
import edu.kit.kastel.vads.compiler.asm.node.operand.RegAsm;
import edu.kit.kastel.vads.compiler.asm.node.operand.RegType;
import edu.kit.kastel.vads.compiler.asm.node.operand.StackAsm;

public class MovAsm implements InstructionAsm {
    private OperandAsm src;
    private OperandAsm dst;

    public MovAsm(OperandAsm src, OperandAsm dst) {
        this.src = src;
        this.dst = dst;
    }

    public OperandAsm src() {
        return src;
    }

    public OperandAsm dst() {
        return dst;
    }

    @Override
    public String toString() {
        if (src instanceof StackAsm && dst instanceof StackAsm) {
            return "movl " + src.toString() + ", " + new RegAsm(RegType.CX).toString() + "\n" +
                   "movl " + new RegAsm(RegType.CX).toString() + ", " + dst.toString();
        } else {
            return "movl " + src.toString() + ", " + dst.toString();
        }
    }

    @Override
    public List<OperandAsm> getOperands() {
        return List.of(src, dst);
    }

    @Override
    public void setOperand(int index, OperandAsm operand) {
        if (index == 0) {
            this.src = operand;
        } else if (index == 1) {
            this.dst = operand;
        }
    }
}
