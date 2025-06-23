package edu.kit.kastel.vads.compiler.ir_tac.node.instruction;

import edu.kit.kastel.vads.compiler.ir_tac.node.val.Val;
import edu.kit.kastel.vads.compiler.ir_tac.node.val.Var;
import edu.kit.kastel.vads.compiler.lexer.Operator.OperatorType;

public class Binary implements Instruction {
    private final OperatorType operator;
    private final Val src1;
    private final Val src2;
    private final Var dst;

    public Binary(OperatorType operator, Val src1, Val src2, Var dst) {
        this.operator = operator;
        this.src1 = src1;
        this.src2 = src2;
        this.dst = dst;
    }

    public OperatorType operator() {
        return operator;
    }

    public Val src1() {
        return src1;
    }

    public Val src2() {
        return src2;
    }

    public Var dst() {
        return dst;
    }
}
