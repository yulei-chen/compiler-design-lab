package edu.kit.kastel.vads.compiler.ir_tac.node;

import java.util.List;

import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Instruction;

public class Function {
    private final String identifier;
    private final List<String> parameters;
    private final List<Instruction> body;

    public Function(String identifier, List<String> parameters, List<Instruction> instructions) {
        this.identifier = identifier;
        this.parameters = parameters;
        this.body = instructions;
    }
    

}
