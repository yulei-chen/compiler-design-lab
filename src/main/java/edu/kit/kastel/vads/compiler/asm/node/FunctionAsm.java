package edu.kit.kastel.vads.compiler.asm.node;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.instruction.InstructionAsm;

public class FunctionAsm {
    private final String name;
    private final List<InstructionAsm> instructions;

    public FunctionAsm(String name, List<InstructionAsm> instructions) {
        this.name = name;
        this.instructions = instructions;
    }
}
