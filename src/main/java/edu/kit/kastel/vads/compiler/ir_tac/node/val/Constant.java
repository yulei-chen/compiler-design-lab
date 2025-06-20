package edu.kit.kastel.vads.compiler.ir_tac.node.val;

public class Constant implements Val {
    private final int value;

    public Constant(int value) {
        this.value = value;
    }

    public Constant(String value) {
        this.value = Integer.parseInt(value);
    }

    public int value() {
        return value;
    }
}
