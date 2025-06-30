package edu.kit.kastel.vads.compiler.asm.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;

public class PrintAsm implements InstructionAsm {
    private final OperandAsm value;

    public PrintAsm(OperandAsm value) {
        this.value = value;
    }

    @Override
    public List<OperandAsm> getOperands() {
        return List.of(value);
    }

    @Override
    public void setOperand(int index, OperandAsm operand) {
        if (index == 0) {
            // 这里我们不能直接修改value，因为它是final的
            // 在实际使用中，寄存器分配器会通过反射或其他方式处理这个问题
            throw new UnsupportedOperationException("Cannot modify operand in PrintAsm");
        }
        throw new IllegalArgumentException("PrintAsm only has one operand at index 0");
    }

    @Override
    public String toString() {
        return String.format(
            "movq %s, %%rdi\n" +  // 将值移动到第一个参数寄存器
            "leaq fmt(%%rip), %%rsi\n" +  // 格式字符串地址作为第二个参数
            "movl $0, %%eax\n" +  // 清空%eax（告诉printf没有浮点参数）
            "call printf",  // 调用printf
            value.toString()
        );
    }
} 