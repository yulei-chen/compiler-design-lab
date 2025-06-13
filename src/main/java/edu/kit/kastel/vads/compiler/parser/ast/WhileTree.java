package edu.kit.kastel.vads.compiler.parser.ast;

import edu.kit.kastel.vads.compiler.Span;
import edu.kit.kastel.vads.compiler.parser.visitor.Visitor;

public final class WhileTree implements StatementTree {
    private final ExpressionTree condition;
    private final StatementTree body;
    private final Span span;

    public WhileTree(ExpressionTree condition, StatementTree body, Span span) {
        this.condition = condition;
        this.body = body;
        this.span = span;
    }

    public ExpressionTree condition() {
        return condition;
    }

    public StatementTree body() {
        return body;
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