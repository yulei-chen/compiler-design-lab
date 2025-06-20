package edu.kit.kastel.vads.compiler.asm.reg_alloc;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import edu.kit.kastel.vads.compiler.asm.node.instruction.InstructionAsm;
import edu.kit.kastel.vads.compiler.asm.node.instruction.MovAsm;
import edu.kit.kastel.vads.compiler.asm.node.instruction.UnaryAsm;
import edu.kit.kastel.vads.compiler.asm.node.operand.PseudoAsm;

/**
 * `liveIn` is the result of analysis.
 */
public class Liveness {

    private List<InstructionAsm> asmInstructions;
    private Set<String> temps;
    private HashMap<Integer, HashSet<String>> liveIn;
    private HashMap<Integer, HashSet<String>> defMap;
    private HashMap<Integer, HashSet<String>> useMap;
    private HashMap<Integer, HashSet<Integer>> succMap;

    public Liveness(List<InstructionAsm> asmInstructions) {
        this.asmInstructions = asmInstructions;
        this.temps = new HashSet<>();
        liveIn = new HashMap<>();   
        defMap = new HashMap<>();
        useMap = new HashMap<>();
        succMap = new HashMap<>();
    }

    public HashMap<Integer, HashSet<String>> backwardAnalyze() {
        for (int i= asmInstructions.size() - 1; i >= 0; i--) {
            InstructionAsm instruction = asmInstructions.get(i);
            formalizeLine(instruction, i);
        // Why not formalize and analyze at the same time in the same loop?
        //     Some results are not calculated when using `goto`
        }

        for (int i= asmInstructions.size() - 1; i >= 0; i--) {
            InstructionAsm instruction = asmInstructions.get(i);
            analyzeLine(instruction, i);
        }

        return liveIn;
    }

    private void formalizeLine(InstructionAsm instruction, Integer lineNumber) {

        // %temp.0 = ...
        // if (instruction instanceof DeclarationAsm) {
        //     if (!defMap.containsKey(lineNumber)) {
        //         defMap.put(lineNumber, new HashSet<>());
        //     }  
        //     defMap.get(lineNumber).add(instruction.dst());
        // }

        // goto ...
        // if (instruction instanceof GotoAsm) {
        //     int succ = Integer.parseInt(instruction.dst());
        //     if (!succMap.containsKey(lineNumber)) {
        //         succMap.put(lineNumber, new HashSet<>());
        //     }
        //     succMap.get(lineNumber).add(succ);
        // } else {
        //     if (!succMap.containsKey(lineNumber)) {
        //         succMap.put(lineNumber, new HashSet<>());
        //     }
        //     succMap.get(lineNumber).add(lineNumber + 1);
        // }

        if (!succMap.containsKey(lineNumber)) {
            succMap.put(lineNumber, new HashSet<>());
        }
        succMap.get(lineNumber).add(lineNumber + 1);


        switch (instruction) {
            case MovAsm movAsm -> {
                if (!useMap.containsKey(lineNumber)) {
                    useMap.put(lineNumber, new HashSet<>());
                }
                if (movAsm.src() instanceof PseudoAsm pseudoAsm) {
                    useMap.get(lineNumber).add(pseudoAsm.identifier());
                    this.temps.add(pseudoAsm.identifier());
                }
                if (movAsm.dst() instanceof PseudoAsm pseudoAsm) {
                    useMap.get(lineNumber).add(pseudoAsm.identifier());
                    this.temps.add(pseudoAsm.identifier());
                }
            }
            case UnaryAsm unaryAsm -> {
                if (!useMap.containsKey(lineNumber)) {
                    useMap.put(lineNumber, new HashSet<>());
                }
                if (unaryAsm.operand() instanceof PseudoAsm pseudoAsm) {
                    useMap.get(lineNumber).add(pseudoAsm.identifier());
                    this.temps.add(pseudoAsm.identifier());
                }
            }
            default -> {
                // do nothing
            }
        }
    }

    private void analyzeLine(InstructionAsm instruction, Integer lineNumber) {
        for (String temp : temps) {
            Boolean isLive = live(lineNumber, temp);
            if (isLive) {
                if (!liveIn.containsKey(lineNumber)) {
                    liveIn.put(lineNumber, new HashSet<>());
                }
                liveIn.get(lineNumber).add(temp);
            }
        }
    }
  
    /*** Predicates  */

    public boolean live(Integer lineNumber, String virtualReg) {
        // Space/time trade off for recursive calls
        if (liveIn.containsKey(lineNumber) && liveIn.get(lineNumber).contains(virtualReg)) {
            return true;
        }
        
        // K1
        if (use(lineNumber, virtualReg)) {
            return true;
        }
        
        // K2
        HashSet<Integer> succSet = succMap.get(lineNumber);
        if (succSet == null) {
            return false;
        }
        for (Integer succ : succSet) {
           if (live(succ, virtualReg) && !def(lineNumber, virtualReg)) {
            return true;
           }
        }

        return false;
    }

    public boolean use(Integer lineNumber, String virtualReg) {
        return useMap.getOrDefault(lineNumber, new HashSet<>()).contains(virtualReg);
    }

    public boolean def(Integer lineNumber, String virtualReg) {
        return defMap.getOrDefault(lineNumber, new HashSet<>()).contains(virtualReg);
    }

}
