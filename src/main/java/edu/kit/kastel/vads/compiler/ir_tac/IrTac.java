package edu.kit.kastel.vads.compiler.ir_tac;

import java.util.ArrayList;
import java.util.List;

import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Binary;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Instruction;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Return;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Unary;
import edu.kit.kastel.vads.compiler.ir_tac.node.val.Constant;
import edu.kit.kastel.vads.compiler.ir_tac.node.val.Val;
import edu.kit.kastel.vads.compiler.ir_tac.node.val.Var;
import edu.kit.kastel.vads.compiler.ir_tac.utils.MakeTemp;
import edu.kit.kastel.vads.compiler.parser.ast.BinaryOperationTree;
import edu.kit.kastel.vads.compiler.parser.ast.BlockTree;
import edu.kit.kastel.vads.compiler.parser.ast.ExpressionTree;
import edu.kit.kastel.vads.compiler.parser.ast.FunctionTree;
import edu.kit.kastel.vads.compiler.parser.ast.LiteralTree;
import edu.kit.kastel.vads.compiler.parser.ast.ProgramTree;
import edu.kit.kastel.vads.compiler.parser.ast.ReturnTree;
import edu.kit.kastel.vads.compiler.parser.ast.StatementTree;
import edu.kit.kastel.vads.compiler.parser.ast.UnaryOperationTree;

/** Intermediate Representation - Three Address Code */
public class IrTac {
    private final Visitor visitor;

    public IrTac(ProgramTree program) {
        this.visitor = new Visitor();
        for (FunctionTree function : program.topLevelTrees()) {
            visitor.visit(function);
        }
    }

    public List<Instruction> instructions() {
        return visitor.instructions;
    }

    public class Visitor {
        private final List<Instruction> instructions;

        public Visitor() {
            this.instructions = new ArrayList<>();
        }

        public void visit(FunctionTree function) {
            visitor.visit(function.body());
        }

        public void visit(BlockTree block) {
            for (StatementTree statement : block.statements()) {
                visitor.visit(statement);
            }
        }

        ////// Statements Start //////

        public void visit(StatementTree statement) {
            if (statement instanceof ReturnTree returnTree) {
                visitor.visit(returnTree);
            } else {
                throw new IllegalArgumentException("Unknown statement type: " + statement.getClass().getName());
            }
        }

        public void visit(ReturnTree returnTree) {
            Val val = visitor.visit(returnTree.expression());
            this.instructions.add(new Return(val));
        }
      
        public Val visit(ExpressionTree expressionTree) {
            switch (expressionTree) {
                case LiteralTree literalTree -> {
                    return new Constant(literalTree.raw());
                }
                case UnaryOperationTree unaryOperationTree -> {
                    return visitor.visit(unaryOperationTree);
                }
                case BinaryOperationTree binaryOperationTree -> {
                    return visitor.visit(binaryOperationTree);
                }
                default -> throw new IllegalArgumentException("Unknown expression type: " + expressionTree.getClass().getName());
            }
        }

        ////// Expressions Start //////

        public Val visit(UnaryOperationTree unaryOperationTree) {
            Val src = visitor.visit(unaryOperationTree.operand());
            String dst_name = MakeTemp.makeTemp();
            Var dst = new Var(dst_name);
            this.instructions.add(new Unary(unaryOperationTree.operatorType(), src, dst));
            return dst;
        }

        public Val visit(BinaryOperationTree binaryOperationTree) {
            Val src1 = visitor.visit(binaryOperationTree.lhs());
            Val src2 = visitor.visit(binaryOperationTree.rhs());
            String dst_name = MakeTemp.makeTemp();
            Var dst = new Var(dst_name);
            this.instructions.add(new Binary(binaryOperationTree.operatorType(), src1, src2, dst));
            return dst;
        }
    }
}
