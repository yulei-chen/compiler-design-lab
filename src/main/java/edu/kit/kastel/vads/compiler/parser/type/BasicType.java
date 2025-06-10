package edu.kit.kastel.vads.compiler.parser.type;

import java.util.Locale;

import edu.kit.kastel.vads.compiler.lexer.KeywordType;

public enum BasicType implements Type {
    INT,
    BOOL;

    public static BasicType fromKeyword(KeywordType keywordType) {
        return switch (keywordType) {
            case KeywordType.INT -> INT;
            case KeywordType.BOOL -> BOOL;
            default -> throw new IllegalArgumentException("Invalid keyword type: " + keywordType);
        };
    }

    @Override
    public String asString() {
        return name().toLowerCase(Locale.ROOT);
    }
}
