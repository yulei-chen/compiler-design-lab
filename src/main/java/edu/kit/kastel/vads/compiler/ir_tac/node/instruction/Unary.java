package edu.kit.kastel.vads.compiler.ir_tac.node.instruction;

import edu.kit.kastel.vads.compiler.ir_tac.node.val.Val;
import edu.kit.kastel.vads.compiler.ir_tac.node.val.Var;
import edu.kit.kastel.vads.compiler.lexer.Operator.OperatorType;

public class Unary implements Instruction {
    private final OperatorType operator;
    private final Val src;
    private final Var dst;

    public Unary(OperatorType operator, Val src, Var dst) {
        this.operator = operator;
        this.src = src;
        this.dst = dst;
    }

    public OperatorType operator() {
        return operator;
    }

    public Val src() {
        return src;
    }

    public Var dst() {
        return dst;
    }
}
