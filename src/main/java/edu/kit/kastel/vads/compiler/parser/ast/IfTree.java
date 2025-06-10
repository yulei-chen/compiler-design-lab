package edu.kit.kastel.vads.compiler.parser.ast;

import edu.kit.kastel.vads.compiler.Span;
import edu.kit.kastel.vads.compiler.parser.visitor.Visitor;

public final class IfTree implements StatementTree {
    private final ExpressionTree condition;
    private final BlockTree thenBlock;
    private final BlockTree elseBlock;
    private final Span span;

    public IfTree(ExpressionTree condition, BlockTree thenBlock, BlockTree elseBlock, Span span) {
        this.condition = condition;
        this.thenBlock = thenBlock;
        this.elseBlock = elseBlock;
        this.span = span;
    }

    public ExpressionTree condition() {
        return condition;
    }

    public BlockTree thenBlock() {
        return thenBlock;
    }

    public BlockTree elseBlock() {
        return elseBlock;
    }

    @Override
    public Span span() {
        return span;
    }

    @Override
    public <T, R> R accept(Visitor<T, R> visitor, T t) {
        return visitor.visit(this, t);
    }
} 