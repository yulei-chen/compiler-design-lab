package edu.kit.kastel.vads.compiler.parser.ast;

import edu.kit.kastel.vads.compiler.Span;
import edu.kit.kastel.vads.compiler.parser.visitor.Visitor;

public final class IfTree implements StatementTree {
    private final ExpressionTree condition;
    private final StatementTree thenStatement;
    private final StatementTree elseStatement;
    private final Span span;

    public IfTree(ExpressionTree condition, StatementTree thenStatement, StatementTree elseStatement, Span span) {
        this.condition = condition;
        this.thenStatement = thenStatement;
        this.elseStatement = elseStatement;
        this.span = span;
    }

    public ExpressionTree condition() {
        return condition;
    }

    public StatementTree thenStatement() {
        return thenStatement;
    }

    public StatementTree elseStatement() {
        return elseStatement;
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