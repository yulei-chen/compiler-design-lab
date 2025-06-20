package edu.kit.kastel.vads.compiler.ir_tac.node.val;

public class Var implements Val {
    private final String identifier;

    public Var(String identifier) {
        this.identifier = identifier;
    }

    public String identifier() {
        return identifier;
    }
}
