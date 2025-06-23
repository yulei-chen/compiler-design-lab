package edu.kit.kastel.vads.compiler.ir_tac.node.instruction;

import edu.kit.kastel.vads.compiler.ir_tac.node.val.Val;
import edu.kit.kastel.vads.compiler.ir_tac.node.val.Var;

public class Copy implements Instruction {
    private final Val src;
    private final Var dst;

    public Copy(Val src, Var dst) {
        this.src = src;
        this.dst = dst;
    }

    public Val src() {
        return src;
    }

    public Var dst() {
        return dst;
    }
}
