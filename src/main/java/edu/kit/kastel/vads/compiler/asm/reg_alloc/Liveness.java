package edu.kit.kastel.vads.compiler.asm.reg_alloc;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import edu.kit.kastel.vads.compiler.asm.node.instruction.InstructionAsm;
import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;
import edu.kit.kastel.vads.compiler.asm.node.operand.PseudoAsm;

/**
 * `liveIn` is the result of this analysis
 */
public class Liveness {
    private List<String> lines;
    public Set<String> virtualRegs;
    public HashMap<Integer, HashSet<String>> liveIn;
    private HashMap<Integer, HashSet<String>> defMap;
    private HashMap<Integer, HashSet<String>> useMap;
    private HashMap<Integer, HashSet<Integer>> succMap;

    public Liveness(List<InstructionAsm> asmInstructions) {
        this.lines = Arrays.asList(asmInstructions.toString().split("\n"));

        this.virtualRegs = new HashSet<>();
        for (InstructionAsm instruction : asmInstructions) {
            for (OperandAsm operand : instruction.getOperands()) {
                if (operand instanceof PseudoAsm pseudoAsm) {
                    virtualRegs.add(pseudoAsm.identifier());
                }
            }
        }
        
        liveIn = new HashMap<>();   
        defMap = new HashMap<>();
        useMap = new HashMap<>();
        succMap = new HashMap<>();
    }

    public HashMap<Integer, HashSet<String>> backwardAnalyze() {
        for (int i= lines.size() - 1; i >= 0; i--) {
            String line = lines.get(i);
            formalizeLine(line, i);
        // Why not formalize and analyze at the same time in the same loop?
        //     Some results are not calculated when using `goto`
        }

        for (int i= lines.size() - 1; i >= 0; i--) {
            String line = lines.get(i);
            analyzeLine(line, i);
        }

        return liveIn;
    }

    private void formalizeLine(String line, Integer lineNumber) {
        String[] parts = line.trim().split(", ");
        
        // %1 = ...
        if (parts[0].startsWith("%")) {
            if (!defMap.containsKey(lineNumber)) {
                defMap.put(lineNumber, new HashSet<>());
            }  
            defMap.get(lineNumber).add(parts[0]);
        }

        // jmp ...
        if (parts[0].equals("jmp")) {
            int succ = getTargetLineNumber(parts[1]);
            if (!succMap.containsKey(lineNumber)) {
                succMap.put(lineNumber, new HashSet<>());
            }
            succMap.get(lineNumber).add(succ);
        // jmpcc ...
        } else if (parts[0].startsWith("j")) {
            int succ = getTargetLineNumber(parts[1]);
            if (!succMap.containsKey(lineNumber)) {
                succMap.put(lineNumber, new HashSet<>());
            }
            succMap.get(lineNumber).add(succ);
            succMap.get(lineNumber).add(lineNumber + 1);
        // other instructions
        } else {
            if (!succMap.containsKey(lineNumber)) {
                succMap.put(lineNumber, new HashSet<>());
            }
            succMap.get(lineNumber).add(lineNumber + 1);
        }

        for (int i = 1; i < parts.length; i++) {
            if (parts[i].matches("%\\.")) {
                if (!useMap.containsKey(lineNumber)) {
                    useMap.put(lineNumber, new HashSet<>());
                }
                useMap.get(lineNumber).add(parts[i]);
            }
        }
    }

    private void analyzeLine(String line, Integer lineNumber) {
        for (String reg : virtualRegs) {
            if (reg.matches("%\\.")) {
               Boolean isLive = live(lineNumber, reg);
               if (isLive) {
                if (!liveIn.containsKey(lineNumber)) {
                    liveIn.put(lineNumber, new HashSet<>());
                }
                liveIn.get(lineNumber).add(reg);
               }
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

    // Util
    public Integer getTargetLineNumber(String target) {
        for (int i= lines.size() - 1; i >= 0; i--) {
            String line = lines.get(i);
            if (line.startsWith(target)) {
                return i;
            }
        }
        return null;
    }
}
