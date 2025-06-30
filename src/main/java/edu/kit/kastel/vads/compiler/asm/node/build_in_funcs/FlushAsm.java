package edu.kit.kastel.vads.compiler.asm.node.build_in_funcs;

public class FlushAsm {
    @Override
    public String toString() {
        return """
.global flush
flush:
    # 保存调用者保存的寄存器
    pushq %rbp
    movq %rsp, %rbp
    
    # 设置系统调用参数
    # fsync(int fd)
    movq $74, %rax   # syscall number for fsync
    movq $1, %rdi    # fd = 1 (stdout)
    
    # 执行系统调用
    syscall
    
    # 返回值设为0
    movq $0, %rax
    
    # 恢复栈帧
    movq %rbp, %rsp
    popq %rbp
    ret
        """;
    }
} 