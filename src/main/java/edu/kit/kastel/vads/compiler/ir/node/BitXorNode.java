package edu.kit.kastel.vads.compiler.ir.node;

public final class BitXorNode extends BinaryOperationNode {
    public BitXorNode(Block block, Node left, Node right) {
        super(block, left, right);
    }

    @Override
    public String toString() {
        return "^";
    }
} 