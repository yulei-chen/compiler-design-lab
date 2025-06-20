package edu.kit.kastel.vads.compiler.asm.reg_alloc;

import java.util.*;


public class InterferenceGraph {
    private final Set<String> nodes = new HashSet<>();
    private final Map<String, Set<String>> edges = new HashMap<>();

    public void addNode(String reg) {
        nodes.add(reg);
        edges.computeIfAbsent(reg, k -> new HashSet<>());
    }

    public void addEdge(String reg1, String reg2) {
        if (reg1.equals(reg2)) return;
        edges.computeIfAbsent(reg1, k -> new HashSet<>()).add(reg2);
        edges.computeIfAbsent(reg2, k -> new HashSet<>()).add(reg1);
    }

    public Set<String> getNodes() {
        return Collections.unmodifiableSet(nodes);
    }

    public Set<String> getNeighbors(String reg) {
        return edges.getOrDefault(reg, Collections.emptySet());
    }

    public int degree(String reg) {
        return getNeighbors(reg).size();
    }
} 