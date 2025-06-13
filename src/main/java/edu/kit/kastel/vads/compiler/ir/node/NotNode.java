package edu.kit.kastel.vads.compiler.ir.node;

public final class NotNode extends Node {
    private final Node operand;

    public NotNode(Block block, Node operand) {
        super(block, operand);
        this.operand = operand;
    }

    public Node operand() {
        return this.operand;
    }
} 