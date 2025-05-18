package edu.kit.kastel.vads.compiler.backend.regalloc;
import java.util.*;

/**
 * 基于图着色的寄存器分配器。
 */
public class GraphColoringRegisterAllocator {
    private static final String[] PHYSICAL_REGS = {
        Register.RAX, Register.RBX, Register.RCX, Register.RDX, Register.RSI, Register.RDI, 
        Register.R8, Register.R9, Register.R10, Register.R11, Register.R12, Register.R13, Register.R14, Register.R15
    };

    /**
     * 输入aasm指令列表，输出虚拟寄存器到物理寄存器的分配结果。
     */
    public Map<String, String> allocate(List<String> aasmLines) {
        Set<String> virtualRegs = collectVirtualRegisters(aasmLines);
        InterferenceGraph graph = buildInterferenceGraph(aasmLines, virtualRegs);
        return colorGraph(graph);
    }

    // 收集所有虚拟寄存器名
    private Set<String> collectVirtualRegisters(List<String> lines) {
        Set<String> regs = new HashSet<>();
        for (String line : lines) {
            for (String token : line.split("[ =]+")) {
                if (token.matches("%\\d+")) {
                    regs.add(token);
                }
            }
        }
        return regs;
    }

    private InterferenceGraph buildInterferenceGraph(List<String> lines, Set<String> regs) {
        InterferenceGraph graph = new InterferenceGraph();
        for (String reg : regs) graph.addNode(reg);

        Liveness liveness = new Liveness(lines, regs);
        HashMap<Integer, HashSet<String>> liveIn = liveness.backwardAnalyze();

        for (Integer lineNumber : liveIn.keySet()) {
            HashSet<String> liveRegsAtTheSameTime = liveIn.get(lineNumber);
            for (String reg1 : liveRegsAtTheSameTime) {
                for (String reg2 : liveRegsAtTheSameTime) {
                    if (reg1.equals(reg2)) continue;
                    graph.addEdge(reg1, reg2);
                }
            }
        }
        return graph;
    }

    // 图着色算法（贪心）
    private Map<String, String> colorGraph(InterferenceGraph graph) {
        Map<String, String> allocation = new HashMap<>();
        List<String> regs = new ArrayList<>(graph.getNodes());
        regs.sort(Comparator.comparingInt(graph::degree).reversed());
        for (String reg : regs) {
            Set<String> neighborColors = new HashSet<>();
            for (String neighbor : graph.getNeighbors(reg)) {
                if (allocation.containsKey(neighbor)) {
                    neighborColors.add(allocation.get(neighbor));
                }
            }
            String assigned = null;
            for (String phys : PHYSICAL_REGS) {
                if (!neighborColors.contains(phys)) {
                    assigned = phys;
                    break;
                }
            }
            if (assigned == null) {
                throw new RuntimeException("寄存器溢出：物理寄存器数量不足");
            }
            allocation.put(reg, assigned);
        }
        return allocation;
    }
} 