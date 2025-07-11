package edu.kit.kastel.vads.compiler.asm.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;
import edu.kit.kastel.vads.compiler.asm.node.operand.RegAsm;
import edu.kit.kastel.vads.compiler.asm.node.operand.RegType;
import edu.kit.kastel.vads.compiler.asm.node.operand.StackAsm;

public class BinaryAsm implements InstructionAsm {
    private BinaryOperator operator;
    private OperandAsm src;
    private OperandAsm dst;

    public BinaryAsm(BinaryOperator operator, OperandAsm src, OperandAsm dst) {
        this.operator = operator;
        this.src = src;
        this.dst = dst;
    }

    @Override
    public String toString() {
        // NOTE: the dst of `imull` instruction can't be stack operands
        if (operator == BinaryOperator.MUL) {
            return "movl " + dst.toString() + ", " + new RegAsm(RegType.CX).toString() + "\n" +
                   "imull " + src.toString() + ", " + new RegAsm(RegType.CX).toString() + "\n" +
                   "movl " + new RegAsm(RegType.CX).toString() + ", " + dst.toString();
        // NOTE: stack can't be used as src and dst at the same time for other binary instructions
        } else if (src instanceof StackAsm && dst instanceof StackAsm) {
            return "movl " + src.toString() + ", " + new RegAsm(RegType.CX).toString() + "\n" +
                   operator.toString() + " " + new RegAsm(RegType.CX).toString() + ", " + dst.toString();
        } else {
            return operator.toString() + " " + src.toString() + ", " + dst.toString();
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
