package edu.kit.kastel.vads.compiler.backend.regalloc;

public interface Register {
    static String RAX = "%rax"; // accumulator
    static String RBX = "%rbx"; // base
    static String RCX = "%rcx"; // counter
    static String RDX = "%rdx"; // data
    static String RSI = "%rsi"; // source index
    static String RDI = "%rdi"; // destination index
    static String R8 = "%r8"; // general purpose
    static String R9 = "%r9"; // general purpose
    static String R10 = "%r10"; // general purpose
    static String R11 = "%r11"; // general purpose
    static String R12 = "%r12"; // general purpose
    static String R13 = "%r13"; // general purpose
    static String R14 = "%r14"; // general purpose
    static String R15 = "%r15"; // general purpose
}
