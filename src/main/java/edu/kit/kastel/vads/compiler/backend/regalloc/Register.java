package edu.kit.kastel.vads.compiler.backend.regalloc;

public interface Register {
    /** 64-bit registers */
    static String RAX = "%rax"; 
    static String RBX = "%rbx"; 
    static String RCX = "%rcx"; 
    static String RDX = "%rdx"; 
    static String RSI = "%rsi"; 
    static String RDI = "%rdi"; 
    static String R8 = "%r8"; 
    static String R9 = "%r9"; 
    static String R10 = "%r10"; 
    static String R11 = "%r11"; 
    static String R12 = "%r12"; 
    static String R13 = "%r13"; 
    static String R14 = "%r14"; 
    static String R15 = "%r15"; 

    /** 32-bit registers */
    static String EAX = "%eax"; 
    static String EBX = "%ebx"; 
    static String ECX = "%ecx"; 
    static String EDX = "%edx"; 
    static String ESI = "%esi"; 
    static String EDI = "%edi"; 
    static String R8D = "%r8d"; 
    static String R9D = "%r9d"; 
    static String R10D = "%r10d"; 
    static String R11D = "%r11d"; 
    static String R12D = "%r12d"; 
    static String R13D = "%r13d"; 
    static String R14D = "%r14d"; 
    static String R15D = "%r15d"; 
}
