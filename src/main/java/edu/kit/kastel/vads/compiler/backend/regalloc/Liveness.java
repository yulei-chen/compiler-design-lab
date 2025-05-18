package edu.kit.kastel.vads.compiler.backend.regalloc;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

public class Liveness {

    private List<String> lines;
    private HashMap<Integer, HashSet<String>> liveIn;
    private HashMap<Integer, HashSet<String>> defMap;
    private HashMap<Integer, HashSet<String>> useMap;
    private HashMap<Integer, HashSet<Integer>> succMap;

    public Liveness(List<String> lines) {
        this.lines = lines;
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
        String[] parts = line.trim().split("[ =]+");
        
        // %1 = ...
        if (parts[0].startsWith("%")) {
            if (!defMap.containsKey(lineNumber)) {
                defMap.put(lineNumber, new HashSet<>());
            }  
            defMap.get(lineNumber).add(parts[0]);
        }

        // goto ...
        if (parts[0].equals("goto")) {
            int succ = Integer.parseInt(parts[1]);
            if (!succMap.containsKey(lineNumber)) {
                succMap.put(lineNumber, new HashSet<>());
            }
            succMap.get(lineNumber).add(succ);

            
        } else if (!parts[0].equals("ret")) {
            if (!succMap.containsKey(lineNumber)) {
                succMap.put(lineNumber, new HashSet<>());
            }
            succMap.get(lineNumber).add(lineNumber + 1);
        }

        for (int i = 1; i < parts.length; i++) {
            if (parts[i].matches("%\\d+")) {
                if (!useMap.containsKey(lineNumber)) {
                    useMap.put(lineNumber, new HashSet<>());
                }
                useMap.get(lineNumber).add(parts[i]);
            }
        }
    }

    private void analyzeLine(String line, Integer lineNumber) {
        String[] parts = line.trim().split("[ =]+");

        for (String reg : parts) {
            if (reg.matches("%\\d+")) {
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
           if (live(succ, virtualReg) && !def(succ, virtualReg)) {
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
