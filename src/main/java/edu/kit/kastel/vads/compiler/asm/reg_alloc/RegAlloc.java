package edu.kit.kastel.vads.compiler.asm.reg_alloc;

import java.util.HashSet;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import edu.kit.kastel.vads.compiler.asm.node.instruction.InstructionAsm;
import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;
import edu.kit.kastel.vads.compiler.asm.node.operand.PseudoAsm;
import edu.kit.kastel.vads.compiler.asm.node.operand.RegAsm;
import edu.kit.kastel.vads.compiler.asm.node.operand.RegType;
import edu.kit.kastel.vads.compiler.asm.node.operand.StackAsm;

public class RegAlloc {
    private final List<InstructionAsm> asmInstructions;
    private final Map<String, OperandAsm> allocation;

    private int stackOffset = 0;
    private final RegAsm[] AVAILABLE_REGS = {
        new RegAsm(RegType.R8),
        new RegAsm(RegType.R9),
        new RegAsm(RegType.R10),
        new RegAsm(RegType.R11),
        new RegAsm(RegType.R12),
        new RegAsm(RegType.R13),
        new RegAsm(RegType.R14),
        new RegAsm(RegType.R15),
    };


    public RegAlloc(List<InstructionAsm> asmInstructions) {
        this.asmInstructions = asmInstructions;
        this.allocation = new HashMap<>();
    }

    public void allocate() {
        InterferenceGraph graph = buildInterferenceGraph();
        colorGraph(graph);
        replaceTempByReg();
    }

    public int stackOffset() {
        return this.stackOffset;
    }

    public InterferenceGraph buildInterferenceGraph() {
        Liveness liveness = new Liveness(asmInstructions);
        // liveness.backwardAnalyze();
        
        InterferenceGraph graph = new InterferenceGraph();

        // Add nodes
        for (String temp : liveness.virtualRegs) {
            graph.addNode(temp);
        }
        
        // Add edges
        for (String reg1 : liveness.virtualRegs) {
            for (String reg2 : liveness.virtualRegs) {
                if (reg1.equals(reg2)) continue;
                graph.addEdge(reg1, reg2);
            }
        }

        return graph;
    }

    private void colorGraph(InterferenceGraph graph) {
        List<String> temps = new ArrayList<>(graph.getNodes());
        temps.sort(Comparator.comparingInt(graph::degree).reversed());
        for (String temp : temps) {
            // Get neighbor colors(registers)
            Set<String> neighborColors = new HashSet<>();
            for (String neighbor : graph.getNeighbors(temp)) {
                if (this.allocation.containsKey(neighbor)) {
                    neighborColors.add(this.allocation.get(neighbor).toString());
                }
            }

            // Assign a color(register) other than the neighbor colors(registers)
            RegAsm assigned = null;
            for (RegAsm reg : this.AVAILABLE_REGS) {
                if (!neighborColors.contains(reg.toString())) {
                    assigned = reg;
                    break;
                }
            }

            // If no color is available, use stack
            if (assigned == null) {
                this.stackOffset -= 4;   
                StackAsm stack = new StackAsm(this.stackOffset);
                this.allocation.put(temp, stack);
            } else {
                this.allocation.put(temp, assigned);
            }
        }
    }
   
    private void replaceTempByReg() {
        for (InstructionAsm instruction : this.asmInstructions) {
            List<OperandAsm> operands = instruction.getOperands();
            for (int i = 0; i < operands.size(); i++) {
                if (operands.get(i) instanceof PseudoAsm pseudoAsm) {
                    OperandAsm regOrStack = this.allocation.get(pseudoAsm.identifier());
                    instruction.setOperand(i, regOrStack);
                }
            }
        }
    }
}
