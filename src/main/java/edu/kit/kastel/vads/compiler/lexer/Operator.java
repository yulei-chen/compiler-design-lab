package edu.kit.kastel.vads.compiler.lexer;

import edu.kit.kastel.vads.compiler.Span;

public record Operator(OperatorType type, Span span) implements Token {

    @Override
    public boolean isOperator(OperatorType operatorType) {
        return type() == operatorType;
    }

    @Override
    public String asString() {
        return type().toString();
    }

    public enum OperatorType {
        /* unary operators */
        NOT("!"),
        COMPLEMENT("~"),
        NEGATE("-"),

        /* assignment operators */
        ASSIGN("="),
        ASSIGN_PLUS("+="),
        ASSIGN_Negate("-="),
        ASSIGN_MUL("*="),
        ASSIGN_DIV("/="),
        ASSIGN_MOD("%="),
        ASSIGN_AND("&="),
        ASSIGN_XOR("^="),
        ASSIGN_OR("|="),
        ASSIGN_SHIFT_LEFT("<<="),
        ASSIGN_SHIFT_RIGHT(">>="),

        /* binary operators */
        PLUS("+"),
        // NEGATE("-"),
        MUL("*"),
        DIV("/"),
        MOD("%"),
        LESS("<"),
        LESS_EQUAL("<="),
        GREATER(">"),
        GREATER_EQUAL(">="),
        EQUAL("=="),
        NOT_EQUAL("!="),
        AND("&&"),
        OR("||"),
        BIT_AND("&"),
        BIT_XOR("^"),
        BIT_OR("|"),
        SHIFT_LEFT("<<"),
        SHIFT_RIGHT(">>"),

        /* ternary operators */
        QUESTION("?"),
        COLON(":"),
        ;

        private final String value;

        OperatorType(String value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return this.value;
        }
    }
}
