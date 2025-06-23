package edu.kit.kastel.vads.compiler.ir_tac.node.instruction;

public class Jump implements Instruction {
    private final String target;

    public Jump(String target) {
        this.target = target;
    }

    public String target() {
        return target;
    }
}
