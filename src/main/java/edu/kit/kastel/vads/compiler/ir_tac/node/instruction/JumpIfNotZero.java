package edu.kit.kastel.vads.compiler.ir_tac.node.instruction;

import edu.kit.kastel.vads.compiler.ir_tac.node.val.Val;

public class JumpIfNotZero implements Instruction {
    private final Val condition;
    private final String target;

    public JumpIfNotZero(Val condition, String target) {
        this.condition = condition;
        this.target = target;
    }

    public Val condition() {
        return condition;
    }

    public String target() {
        return target;
    }
}
