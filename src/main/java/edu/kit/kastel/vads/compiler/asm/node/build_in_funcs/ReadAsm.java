package edu.kit.kastel.vads.compiler.asm.node.build_in_funcs;

public class ReadAsm {
    @Override
    public String toString() {
        return """
.global read
read:
    # 保存调用者保存的寄存器
    pushq %rbp
    movq %rsp, %rbp
    
    # 设置系统调用参数
    # read(int fd, void *buf, size_t count)
    movq $0, %rax    # syscall number for read
    movq $0, %rdi    # fd = 0 (stdin)
    movq $input_buffer, %rsi  # buffer address
    movq $1, %rdx    # count = 1 (read one byte)
    
    # 执行系统调用
    syscall
    
    # 检查返回值
    cmpq $0, %rax    # 比较返回值与0
    jle no_input     # 如果 <= 0，跳转到no_input
    
    # 有输入，读取字符值
    movzbl input_buffer, %eax  # 零扩展字节到32位
    jmp read_done
    
no_input:
    # 没有输入，返回-1
    movq $-1, %rax
    
read_done:
    # 恢复栈帧
    movq %rbp, %rsp
    popq %rbp
    ret

input_buffer:
    .space 1
        """;
    }
} 