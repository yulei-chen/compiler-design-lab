package edu.kit.kastel.vads.compiler.asm.node.operand;

public class RegAsm implements OperandAsm {
    private final RegType reg;
    
    public RegAsm(RegType type) {
        this.reg = type;
    }

    @Override
    public String toString() {
        return reg.toString();
    }
}
