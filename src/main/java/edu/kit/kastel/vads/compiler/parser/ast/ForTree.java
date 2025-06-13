package edu.kit.kastel.vads.compiler.parser.ast;

import edu.kit.kastel.vads.compiler.Span;
import edu.kit.kastel.vads.compiler.parser.visitor.Visitor;

public final class ForTree implements StatementTree {
    private final StatementTree init;
    private final ExpressionTree condition;
    private final StatementTree step;
    private final StatementTree body;
    private final Span span;

    public ForTree(StatementTree init, ExpressionTree condition, StatementTree step, StatementTree body, Span span) {
        this.init = init;
        this.condition = condition;
        this.step = step;
        this.body = body;
        this.span = span;
    }

    public StatementTree init() {
        return init;
    }

    public ExpressionTree condition() {
        return condition;
    }

    public StatementTree step() {
        return step;
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