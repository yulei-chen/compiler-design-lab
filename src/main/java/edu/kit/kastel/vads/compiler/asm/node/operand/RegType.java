package edu.kit.kastel.vads.compiler.asm.node.operand;

public enum RegType {
    AX,
    BX,
    CX,
    DX,
    SI,
    DI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15;

    @Override
    public String toString() {
        switch (this) {
            case AX -> {
                return "%eax";
            }
            case BX -> {
                return "%ebx";
            }
            case CX -> {
                return "%ecx";
            }
            case DX -> {
                return "%edx";
            }
            case SI -> {
                return "%esi";
            }
            case DI -> {
                return "%edi";
            }
            case R8 -> {
                return "%r8d";
            }
            case R9 -> {
                return "%r9d";
            }
            case R10 -> {
                return "%r10d";
            }
            case R11 -> {
                return "%r11d";
            }
            case R12 -> {
                return "%r12d";
            }
            case R13 -> {
                return "%r13d";
            }
            case R14 -> {
                return "%r14d";
            }
            case R15 -> {
                return "%r15d";
            }
        }
        throw new IllegalStateException();
    }
}
