package edu.kit.kastel.vads.compiler.ir.node;

public final class ForNode extends Node {
    private final Node condition;
    private final Block body;

    public ForNode(Block block, Node condition, Block body) {
        super(block, condition);
        this.condition = condition;
        this.body = body;
    }

    public Node condition() {
        return this.condition;
    }

    public Block body() {
        return this.body;
    }
} 