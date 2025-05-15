package edu.kit.kastel.vads.compiler.backend.regalloc;

import java.util.*;

/**
 * 冲突图（Interference Graph），用于寄存器分配的图着色算法。
 */
public class InterferenceGraph {
    // 图的节点：虚拟寄存器名
    private final Set<String> nodes = new HashSet<>();
    // 邻接表：每个寄存器与哪些寄存器冲突
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