package edu.kit.kastel.vads.compiler.asm.node.operand;

import java.util.OptionalLong;

public class ImmAsm implements OperandAsm {
    private final OptionalLong value;

    public ImmAsm(OptionalLong value) {
        this.value = value;
    }

    public OptionalLong value() {
        return value;
    }

    @Override
    public String toString() {
        return "$" + value.orElse(0L);
    }
}