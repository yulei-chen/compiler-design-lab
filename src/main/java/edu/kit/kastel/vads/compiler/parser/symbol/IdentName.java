package edu.kit.kastel.vads.compiler.parser.symbol;

public final class IdentName implements Name {   
    private String identifier;
    
    public IdentName(String identifier) {
        this.identifier = identifier;
    }
    
    public String identifier() {
        return identifier;
    }
    
    @Override
    public String asString() {
        return identifier();
    }

    public void rename(String newName) {
        this.identifier = newName;
    }
}
