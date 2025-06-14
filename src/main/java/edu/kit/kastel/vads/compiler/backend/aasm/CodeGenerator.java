package edu.kit.kastel.vads.compiler.backend.aasm;

import edu.kit.kastel.vads.compiler.backend.regalloc.Register;
import edu.kit.kastel.vads.compiler.ir.IrGraph;
import edu.kit.kastel.vads.compiler.ir.node.AddNode;
import edu.kit.kastel.vads.compiler.ir.node.BinaryOperationNode;
import edu.kit.kastel.vads.compiler.ir.node.Block;
import edu.kit.kastel.vads.compiler.ir.node.ConstIntNode;
import edu.kit.kastel.vads.compiler.ir.node.DivNode;
import edu.kit.kastel.vads.compiler.ir.node.IfNode;
import edu.kit.kastel.vads.compiler.ir.node.ModNode;
import edu.kit.kastel.vads.compiler.ir.node.MulNode;
import edu.kit.kastel.vads.compiler.ir.node.Node;
import edu.kit.kastel.vads.compiler.ir.node.Phi;
import edu.kit.kastel.vads.compiler.ir.node.ProjNode;
import edu.kit.kastel.vads.compiler.ir.node.ReturnNode;
import edu.kit.kastel.vads.compiler.ir.node.StartNode;
import edu.kit.kastel.vads.compiler.ir.node.SubNode;
import edu.kit.kastel.vads.compiler.ir.node.WhileNode;
import edu.kit.kastel.vads.compiler.ir.node.ForNode;
import edu.kit.kastel.vads.compiler.ir.node.NotNode;
import edu.kit.kastel.vads.compiler.ir.node.BitNotNode;
import edu.kit.kastel.vads.compiler.ir.node.ShiftLeftNode;
import edu.kit.kastel.vads.compiler.ir.node.ShiftRightNode;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static edu.kit.kastel.vads.compiler.ir.util.NodeSupport.predecessorSkipProj;

public class CodeGenerator {

    public String generateCode(List<IrGraph> program) {
        StringBuilder builder = new StringBuilder();
        for (IrGraph graph : program) {
            AasmRegisterAllocator allocator = new AasmRegisterAllocator();
            Map<Node, Register> registers = allocator.allocateRegisters(graph);
            builder.append("function ")
                .append(graph.name())
                .append(" {\n");
            generateForGraph(graph, builder, registers);
            builder.append("}");
        }
        return builder.toString();
    }

    private void generateForGraph(IrGraph graph, StringBuilder builder, Map<Node, Register> registers) {
        Set<Node> visited = new HashSet<>();
        scan(graph.endBlock(), visited, builder, registers);
    }

    private void scan(Node node, Set<Node> visited, StringBuilder builder, Map<Node, Register> registers) {
        for (Node predecessor : node.predecessors()) {
            if (visited.add(predecessor)) {
                scan(predecessor, visited, builder, registers);
            }
        }

        switch (node) {
            case AddNode add -> binary(builder, registers, add, "add");
            case SubNode sub -> binary(builder, registers, sub, "sub");
            case MulNode mul -> binary(builder, registers, mul, "mul");
            case DivNode div -> binary(builder, registers, div, "div");
            case ModNode mod -> binary(builder, registers, mod, "mod");
            case ShiftLeftNode shiftLeft -> binary(builder, registers, shiftLeft, "shl");
            case ShiftRightNode shiftRight -> binary(builder, registers, shiftRight, "shr");
            case ReturnNode r -> builder.repeat(" ", 2).append("ret ")
                .append(registers.get(predecessorSkipProj(r, ReturnNode.RESULT)));
            case ConstIntNode c -> builder.repeat(" ", 2)
                .append(registers.get(c))
                .append(" = const ")
                .append(c.value());
            case IfNode ifNode -> {
                builder.repeat(" ", 2)
                    .append("if ")
                    .append(registers.get(ifNode.condition()))
                    .append(" then ");
                if (ifNode.elseNode() != null) {
                    builder.append("else ");
                }
            }
            case WhileNode whileNode -> {
                builder.repeat(" ", 2)
                    .append("while ")
                    .append(registers.get(whileNode.condition()));
            }
            case ForNode forNode -> {
                builder.repeat(" ", 2)
                    .append("for ")
                    .append(registers.get(forNode.condition()));
            }
            case NotNode not -> {
                builder.repeat(" ", 2)
                    .append(registers.get(not))
                    .append(" = not ")
                    .append(registers.get(not.operand()));
            }
            case BitNotNode bitNot -> {
                builder.repeat(" ", 2)
                    .append(registers.get(bitNot))
                    .append(" = bitnot ")
                    .append(registers.get(bitNot.operand()));
            }
            case Phi phi -> {
                builder.repeat(" ", 2)
                    .append(registers.get(phi))
                    .append(" = phi ");
                for (int i = 0; i < phi.predecessors().size(); i++) {
                    if (i > 0) {
                        builder.append(", ");
                    }
                    builder.append(registers.get(phi.predecessor(i)));
                }
            }
            case Block _, ProjNode _, StartNode _ -> {
                // do nothing, skip line break
                return;
            }
            default -> throw new UnsupportedOperationException("Unsupported node type: " + node.getClass().getSimpleName());
        }
        builder.append("\n");
    }

    private static void binary(
        StringBuilder builder,
        Map<Node, Register> registers,
        BinaryOperationNode node,
        String opcode
    ) {
        builder.repeat(" ", 2).append(registers.get(node))
            .append(" = ")
            .append(opcode)
            .append(" ")
            .append(registers.get(predecessorSkipProj(node, BinaryOperationNode.LEFT)))
            .append(" ")
            .append(registers.get(predecessorSkipProj(node, BinaryOperationNode.RIGHT)));
    }
}
