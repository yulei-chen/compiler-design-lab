package edu.kit.kastel.vads.compiler.asm.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;

public class CallPrintf implements InstructionAsm {
    @Override
    public List<OperandAsm> getOperands() {
        return List.of();
    }

    @Override
    public void setOperand(int index, OperandAsm operand) {
        throw new IllegalArgumentException("Printf has no operands");
    }

    @Override
    public String toString() {
        return "leaq fmt(%rip), %rdi\n" + // 格式字符串的地址传给 %rdi
               "movl $0, %eax\n" + // 清空 %eax（告诉 printf 没有浮点参数）
               "call printf\n" + // 调用 printf
               "movl $0, %eax"; // 返回 0
    }
}
