package edu.kit.kastel.vads.compiler.parser.ast;

import edu.kit.kastel.vads.compiler.Span;
import edu.kit.kastel.vads.compiler.parser.visitor.Visitor;

public final class ConditionalTree implements ExpressionTree {
    private final ExpressionTree condition;
    private final ExpressionTree trueBranch;
    private final ExpressionTree falseBranch;
    private final Span span;

    public ConditionalTree(ExpressionTree condition, ExpressionTree trueBranch, ExpressionTree falseBranch, Span span) {
        this.condition = condition;
        this.trueBranch = trueBranch;
        this.falseBranch = falseBranch;
        this.span = span;
    }

    public ExpressionTree condition() {
        return condition;
    }

    public ExpressionTree trueBranch() {
        return trueBranch;
    }

    public ExpressionTree falseBranch() {
        return falseBranch;
    }

    @Override
    public Span span() {
        return span;
    }

    @Override
    public <T, R> R accept(Visitor<T, R> visitor, T data) {
        return visitor.visit(this, data);
    }
} 