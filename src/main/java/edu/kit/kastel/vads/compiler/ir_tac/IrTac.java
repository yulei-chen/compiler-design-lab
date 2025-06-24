package edu.kit.kastel.vads.compiler.ir_tac;

import java.util.ArrayList;
import java.util.List;
import java.util.OptionalLong;

import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Binary;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Copy;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Instruction;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Jump;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.JumpIfNotZero;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.JumpIfZero;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Label;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Return;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Unary;
import edu.kit.kastel.vads.compiler.ir_tac.node.val.Constant;
import edu.kit.kastel.vads.compiler.ir_tac.node.val.Val;
import edu.kit.kastel.vads.compiler.ir_tac.node.val.Var;
import edu.kit.kastel.vads.compiler.ir_tac.utils.Utils;
import edu.kit.kastel.vads.compiler.lexer.Operator.OperatorType;
import edu.kit.kastel.vads.compiler.parser.ast.AssignmentTree;
import edu.kit.kastel.vads.compiler.parser.ast.BinaryOperationTree;
import edu.kit.kastel.vads.compiler.parser.ast.BlockTree;
import edu.kit.kastel.vads.compiler.parser.ast.ConditionalTree;
import edu.kit.kastel.vads.compiler.parser.ast.DeclarationTree;
import edu.kit.kastel.vads.compiler.parser.ast.ExpressionTree;
import edu.kit.kastel.vads.compiler.parser.ast.FunctionTree;
import edu.kit.kastel.vads.compiler.parser.ast.IdentExpressionTree;
import edu.kit.kastel.vads.compiler.parser.ast.IfTree;
import edu.kit.kastel.vads.compiler.parser.ast.LValueIdentTree;
import edu.kit.kastel.vads.compiler.parser.ast.LValueTree;
import edu.kit.kastel.vads.compiler.parser.ast.LiteralTree;
import edu.kit.kastel.vads.compiler.parser.ast.NameTree;
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
            switch (statement) {
                /* simp (end with `;` ) */
                case AssignmentTree assignmentTree -> {
                    visitor.visit(assignmentTree);
                }
                case DeclarationTree declarationTree -> {
                    visitor.visit(declarationTree);
                }

                /* control  */
                case IfTree ifTree -> {
                    visitor.visit(ifTree);
                }
                case ReturnTree returnTree -> {
                    visitor.visit(returnTree);
                }

                /* block */
                case BlockTree blockTree -> {
                    visitor.visit(blockTree);
                }
                default -> {
                    throw new IllegalArgumentException("Unknown statement type: " + statement.getClass().getName());
                }
            }
        }

        public void visit(ReturnTree returnTree) {
            Val val = visitor.visit(returnTree.expression());
            this.instructions.add(new Return(val));
        }

        public void visit(AssignmentTree assignmentTree) {
            Val rhs = visitor.visit(assignmentTree.expression());
            Var lhs = visitor.visit(assignmentTree.lValue());
            OperatorType operatorType = assignmentTree.operator().type();
            switch (operatorType) {
                case ASSIGN -> {
                    this.instructions.add(new Copy(rhs, lhs));
                }
                case ASSIGN_PLUS -> {
                    this.instructions.add(new Binary(OperatorType.PLUS, lhs, rhs, lhs));
                }
                case ASSIGN_NEGATE -> {
                    this.instructions.add(new Binary(OperatorType.NEGATE, lhs, rhs, lhs));
                }
                case ASSIGN_MUL -> {
                    this.instructions.add(new Binary(OperatorType.MUL, lhs, rhs, lhs));
                }
                case ASSIGN_DIV -> {
                    this.instructions.add(new Binary(OperatorType.DIV, lhs, rhs, lhs));
                }
                case ASSIGN_MOD -> {
                    this.instructions.add(new Binary(OperatorType.MOD, lhs, rhs, lhs));
                }
                case ASSIGN_AND -> {
                    this.instructions.add(new Binary(OperatorType.AND, lhs, rhs, lhs));
                }
                default -> {
                    throw new IllegalArgumentException("Unknown assignment operator: " + operatorType);
                }
            }
        }

        public Var visit(LValueTree lValueTree) {
            switch (lValueTree) {
                case LValueIdentTree lValueIdentTree -> {
                    return visitor.visit(lValueIdentTree.name());
                }
                default -> throw new IllegalArgumentException("Unknown lvalue type: " + lValueTree.getClass().getName());
            }
        }

        public Var visit(NameTree nameTree) {
            return new Var(nameTree.name().asString());
        }

        public void visit(DeclarationTree declarationTree) {
            // NOTE: "declare" is not an instruction in IR/Asm, we regard it as a assignment
            Val rhs = visitor.visit(declarationTree.initializer());
            Var dst = visitor.visit(declarationTree.name());
            this.instructions.add(new Copy(rhs, dst));
        }

        public void visit(IfTree ifTree) {
            if (ifTree.elseStatement() == null) {
                // Only if
                String endLabel = Utils.makeLabel();

                Val condition = visitor.visit(ifTree.condition());
                this.instructions.add(new JumpIfZero(condition, endLabel));
                visitor.visit(ifTree.thenStatement());
                this.instructions.add(new Label(endLabel));
            } else {
                // If-Else
                String elseLabel = Utils.makeLabel();
                String endLabel = Utils.makeLabel();

                Val condition = visitor.visit(ifTree.condition());
                this.instructions.add(new JumpIfZero(condition, elseLabel));
                visitor.visit(ifTree.thenStatement());
                this.instructions.add(new Jump(endLabel));
                this.instructions.add(new Label(elseLabel));
                visitor.visit(ifTree.elseStatement());
                this.instructions.add(new Label(endLabel));
            }
        }

        ////// Expressions Start //////
        
        public Val visit(ExpressionTree expressionTree) {
            switch (expressionTree) {
                case LiteralTree literalTree -> {
                    return new Constant(literalTree.parseValue());
                }
                case UnaryOperationTree unaryOperationTree -> {
                    return visitor.visit(unaryOperationTree);
                }
                case BinaryOperationTree binaryOperationTree -> {
                    return visitor.visit(binaryOperationTree);
                }
                case IdentExpressionTree identExpressionTree -> {
                    return new Var(identExpressionTree.name().name().asString());
                }
                case ConditionalTree conditionalTree -> {
                    return visitor.visit(conditionalTree);
                }
                default -> throw new IllegalArgumentException("Unknown expression type: " + expressionTree.getClass().getName());
            }
        }

        public Val visit(UnaryOperationTree unaryOperationTree) {
            Val src = visitor.visit(unaryOperationTree.operand());
            String dst_name = Utils.makeTemp();
            Var dst = new Var(dst_name);
            this.instructions.add(new Unary(unaryOperationTree.operatorType(), src, dst));
            return dst;
        }

        public Val visit(BinaryOperationTree binaryOperationTree) {
            OperatorType operatorType = binaryOperationTree.operatorType();

            switch (operatorType) {
                case AND: {
                    Val src1 = visitor.visit(binaryOperationTree.lhs());
                    String falseLabel = Utils.makeLabel();
                    // short circuiting
                    this.instructions.add(new JumpIfZero(src1, falseLabel));
                    Val src2 = visitor.visit(binaryOperationTree.rhs());
                    this.instructions.add(new JumpIfZero(src2, falseLabel));
                    String dst_name = Utils.makeTemp();
                    Var dst = new Var(dst_name);
                    this.instructions.add(new Copy(new Constant(OptionalLong.of(1)), dst));
                    String endLabel = Utils.makeLabel();
                    this.instructions.add(new Jump(endLabel));
                    this.instructions.add(new Label(falseLabel));
                    this.instructions.add(new Copy(new Constant(OptionalLong.of(0)), dst));
                    this.instructions.add(new Label(endLabel));
                    return dst;
                }
                case OR: {
                    Val src1 = visitor.visit(binaryOperationTree.lhs());
                    String trueLabel = Utils.makeLabel();
                    // short circuiting
                    this.instructions.add(new JumpIfNotZero(src1, trueLabel));
                    Val src2 = visitor.visit(binaryOperationTree.rhs());
                    this.instructions.add(new JumpIfNotZero(src2, trueLabel));
                    String dst_name = Utils.makeTemp();
                    Var dst = new Var(dst_name);
                    this.instructions.add(new Copy(new Constant(OptionalLong.of(0)), dst));
                    String endLabel = Utils.makeLabel();
                    this.instructions.add(new Jump(endLabel));
                    this.instructions.add(new Label(trueLabel));
                    this.instructions.add(new Copy(new Constant(OptionalLong.of(1)), dst));
                    this.instructions.add(new Label(endLabel));
                    return dst;
                }
                default: {
                    Val src1 = visitor.visit(binaryOperationTree.lhs());
                    Val src2 = visitor.visit(binaryOperationTree.rhs());
                    String dst_name = Utils.makeTemp();
                    Var dst = new Var(dst_name);
                    this.instructions.add(new Binary(binaryOperationTree.operatorType(), src1, src2, dst));
                    return dst;
                }
            }
        }

        public Val visit(ConditionalTree conditionalTree) {
            String falseLabel = Utils.makeLabel();
            String endLabel = Utils.makeLabel();
            Var result = new Var(Utils.makeTemp());

            Val condition = visitor.visit(conditionalTree.condition());
            this.instructions.add(new JumpIfZero(condition, falseLabel));
            Val trueBranch = visitor.visit(conditionalTree.trueBranch());
            this.instructions.add(new Copy(trueBranch, result));
            this.instructions.add(new Jump(endLabel));
            this.instructions.add(new Label(falseLabel));
            Val falseBranch = visitor.visit(conditionalTree.falseBranch());
            this.instructions.add(new Copy(falseBranch, result));
            this.instructions.add(new Label(endLabel));
            return result;
        }
    }
}
