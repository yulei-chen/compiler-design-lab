package edu.kit.kastel.vads.compiler.ir.node;

public final class BitAndNode extends BinaryOperationNode {
    public BitAndNode(Block block, Node left, Node right) {
        super(block, left, right);
    }

    @Override
    public String toString() {
        return "&";
    }
} 