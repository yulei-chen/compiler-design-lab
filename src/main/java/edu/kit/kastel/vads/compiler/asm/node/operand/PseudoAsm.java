package edu.kit.kastel.vads.compiler.asm.node.operand;

public class PseudoAsm implements OperandAsm {
    private final String identifier;

    public PseudoAsm(String identifier) {
        this.identifier = identifier;
    }

    public String identifier() {
        return identifier;
    }

    @Override
    public String toString() {
        return identifier;
    }
}
