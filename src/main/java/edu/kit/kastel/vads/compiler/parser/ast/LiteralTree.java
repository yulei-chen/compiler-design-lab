package edu.kit.kastel.vads.compiler.parser.ast;

import edu.kit.kastel.vads.compiler.Span;
import edu.kit.kastel.vads.compiler.parser.visitor.Visitor;
import java.util.Optional;
import java.util.OptionalLong;

public record LiteralTree(String raw, Span span, Optional<Integer> base) implements ExpressionTree {
    @Override
    public <T, R> R accept(Visitor<T, R> visitor, T data) {
        return visitor.visit(this, data);
    }

    public OptionalLong parseValue() {
        if (raw.equals("true") || raw.equals("false")) {
            return OptionalLong.of(raw.equals("true") ? 1 : 0);
        }
        
        int end = raw.length();
        return switch (base.orElse(10)) {  
            case 16 -> parseHex(end);
            case 10 -> parseDec(end);
            default -> throw new IllegalArgumentException("unexpected base " + base);
        };
    }

    private OptionalLong parseDec(int end) {
        long l;
        try {
            l = Long.parseLong(raw, 0, end, base.orElse(10));
        } catch (NumberFormatException _) {
            return OptionalLong.empty();
        }
        if (l < 0 || l > Integer.toUnsignedLong(Integer.MIN_VALUE)) {
            return OptionalLong.empty();
        }
        return OptionalLong.of(l);
    }

    private OptionalLong parseHex(int end) {
        try {
            return OptionalLong.of(Integer.parseUnsignedInt(raw, 2, end, 16));
        } catch (NumberFormatException e) {
            return OptionalLong.empty();
        }
    }

}
