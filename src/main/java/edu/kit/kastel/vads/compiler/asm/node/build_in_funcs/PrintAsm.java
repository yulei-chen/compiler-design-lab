package edu.kit.kastel.vads.compiler.asm.node.build_in_funcs;

public class PrintAsm {
    @Override
    public String toString() {
        return """
.global print
print:
    # 保存调用者保存的寄存器
    pushq %rbp
    movq %rsp, %rbp
    
    # 第一个参数已经在 %rdi 中（整数）
    # 设置 printf 的格式字符串
    movq $format_string, %rsi
    movq %rdi, %rdx
    
    # 调用 printf
    movq $0, %rax  # 设置浮点参数数量为0
    call printf
    
    # 返回值设为0
    movq $0, %rax
    
    # 恢复栈帧
    movq %rbp, %rsp
    popq %rbp
    ret

format_string:
    .string "%d\\n"
        """;
    }
}
