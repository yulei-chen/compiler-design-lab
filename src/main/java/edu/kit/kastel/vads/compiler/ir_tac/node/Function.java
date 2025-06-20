package edu.kit.kastel.vads.compiler.ir_tac.node;

import java.util.List;

import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Instruction;

public class Function {
    private final String identifier;
    private final List<Instruction> body;

    public Function(String identifier, List<Instruction> instructions) {
        this.identifier = identifier;
        this.body = instructions;
    }
    

}
