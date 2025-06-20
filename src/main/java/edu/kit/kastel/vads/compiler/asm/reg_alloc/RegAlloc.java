package edu.kit.kastel.vads.compiler.asm.reg_alloc;

import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.instruction.InstructionAsm;

public class RegAlloc {
    private final List<InstructionAsm> asmInstructions;

    public RegAlloc(List<InstructionAsm> asmInstructions) {
        this.asmInstructions = asmInstructions;
    }

    public buildInterferenceGraph() {
        Liveness liveness = new Liveness(asmInstructions);
        
        return graph;
    }

   
}
