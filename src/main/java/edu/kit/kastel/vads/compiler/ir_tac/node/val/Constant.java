package edu.kit.kastel.vads.compiler.ir_tac.node.val;

import java.util.OptionalLong;

public class Constant implements Val {
    private final OptionalLong value;

    public Constant(OptionalLong value) {
        this.value = value;
    }

    public OptionalLong value() {
        return value;
    }
}
