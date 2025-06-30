package edu.kit.kastel.vads.compiler.asm.node.operand;

public class RegAsm implements OperandAsm {
    private final RegType reg;
    
    public RegAsm(RegType type) {
        this.reg = type;
    }

    public String toOneByteString() {
        return reg.toOneByteString();
    }

    public String to8ByteString() {
        return reg.to8ByteString();
    }

    @Override
    public String toString() {
        return reg.toString();
    }
}
