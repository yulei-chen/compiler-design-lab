package edu.kit.kastel.vads.compiler.ir.node;

public final class IfNode extends Node {
    private final Node condition;
    private final Node then;
    private final Node elseNode;

    public IfNode(Block block, Node condition, Node then, Node elseNode) {
        super(block);
        this.condition = condition;
        this.then = then;
        this.elseNode = elseNode;
    }

    public Node condition() {
        return condition;
    }

    public Node then() {
        return then;
    }

    public Node elseNode() {
        return elseNode;
    }
} 