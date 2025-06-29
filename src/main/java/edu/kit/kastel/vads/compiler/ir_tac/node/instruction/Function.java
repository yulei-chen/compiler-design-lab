package edu.kit.kastel.vads.compiler.ir_tac.node.instruction;

import java.util.List;

public class Function implements Instruction {
    private final String identifier;
    private final List<String> parameters;

    public Function(String identifier, List<String> parameters) {
        this.identifier = identifier;
        this.parameters = parameters;
    }

    public String name() {
        return identifier;
    }

    public List<String> parameters() {
        return parameters;
    }
}
