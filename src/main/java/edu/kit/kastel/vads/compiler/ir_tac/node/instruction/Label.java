package edu.kit.kastel.vads.compiler.ir_tac.node.instruction;

public class Label implements Instruction {
    private final String name;

    public Label(String name) {
        this.name = name;
    }

    public String name() {
        return name;
    }
}
