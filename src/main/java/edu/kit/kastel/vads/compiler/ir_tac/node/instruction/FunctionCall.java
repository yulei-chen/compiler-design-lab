package edu.kit.kastel.vads.compiler.ir_tac.node.instruction;

import java.util.List;

import edu.kit.kastel.vads.compiler.ir_tac.node.val.Val;
import edu.kit.kastel.vads.compiler.ir_tac.node.val.Var;

public class FunctionCall implements Instruction {
    private final String name;
    private final List<Val> args;
    private final Var dst;


    public FunctionCall(String name, List<Val> args, Var dst) {
        this.name = name;
        this.args = args;
        this.dst = dst;
    }

    public String name() {
        return name;
    }

    public List<Val> args() {
        return args;
    }
    
    public Var dst() {
        return dst;
    }
}
