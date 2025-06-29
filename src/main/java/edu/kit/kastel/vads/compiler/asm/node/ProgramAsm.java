package edu.kit.kastel.vads.compiler.asm.node;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.instruction.FunctionAsm;

public class ProgramAsm {
    private final List<FunctionAsm> functions;

    public ProgramAsm(List<FunctionAsm> functions) {
        this.functions = functions;
    }
}
