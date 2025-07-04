package edu.kit.kastel.vads.compiler.parser.symbol;

import edu.kit.kastel.vads.compiler.lexer.KeywordType;

public record KeywordName(KeywordType type) implements Name {
    @Override
    public String asString() {
        return type().keyword();
    }
}
