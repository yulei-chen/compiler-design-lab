package edu.kit.kastel.vads.compiler.ir_tac.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.ir_tac.node.val.Var;

public class Function implements Instruction {
    private final String identifier;
    private final List<Var> parameters;

    public Function(String identifier, List<Var> parameters) {
        this.identifier = identifier;
        this.parameters = parameters;
    }

    public String name() {
        return identifier;
    }

    public List<Var> parameters() {
        return parameters;
    }
}
