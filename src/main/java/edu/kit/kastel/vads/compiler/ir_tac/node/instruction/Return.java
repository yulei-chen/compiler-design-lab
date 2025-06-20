package edu.kit.kastel.vads.compiler.ir_tac.node.instruction;

import edu.kit.kastel.vads.compiler.ir_tac.node.val.Val;

public class Return implements Instruction {
    private final Val value;

    public Return(Val value) {
        this.value = value;
    }

    public Val value() {
        return value;
    }
}
