package edu.kit.kastel.vads.compiler.ir_tac;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.OptionalLong;

import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Binary;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Copy;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Function;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.FunctionCall;
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
import edu.kit.kastel.vads.compiler.parser.ast.BreakTree;
import edu.kit.kastel.vads.compiler.parser.ast.ConditionalTree;
import edu.kit.kastel.vads.compiler.parser.ast.ContinueTree;
import edu.kit.kastel.vads.compiler.parser.ast.DeclarationTree;
import edu.kit.kastel.vads.compiler.parser.ast.ExpressionTree;
import edu.kit.kastel.vads.compiler.parser.ast.ForTree;
import edu.kit.kastel.vads.compiler.parser.ast.FunctionCallTree;
import edu.kit.kastel.vads.compiler.parser.ast.FunctionTree;
import edu.kit.kastel.vads.compiler.parser.ast.IdentExpressionTree;
import edu.kit.kastel.vads.compiler.parser.ast.IfTree;
import edu.kit.kastel.vads.compiler.parser.ast.LValueIdentTree;
import edu.kit.kastel.vads.compiler.parser.ast.LValueTree;
import edu.kit.kastel.vads.compiler.parser.ast.LiteralTree;
import edu.kit.kastel.vads.compiler.parser.ast.NameTree;
import edu.kit.kastel.vads.compiler.parser.ast.ParamTree;
import edu.kit.kastel.vads.compiler.parser.ast.ProgramTree;
import edu.kit.kastel.vads.compiler.parser.ast.ReturnTree;
import edu.kit.kastel.vads.compiler.parser.ast.StatementTree;
import edu.kit.kastel.vads.compiler.parser.ast.UnaryOperationTree;
import edu.kit.kastel.vads.compiler.parser.ast.WhileTree;
import edu.kit.kastel.vads.compiler.parser.symbol.Name;

/** Intermediate Representation - Three Address Code */
public class IrTac {
    private final Visitor visitor;
    private final List<List<Instruction>> instructionMatrix;

    public IrTac(ProgramTree program) {
        this.instructionMatrix = new ArrayList<>();
        this.visitor = new Visitor();
        for (FunctionTree function : program.topLevelTrees()) {
            List<Instruction> instructions = visitor.visit(function);
            this.instructionMatrix.add(instructions);
        }
    }

    public List<List<Instruction>> instructionMatrix() {
        return this.instructionMatrix;
    }

    public class Visitor {
        private List<Instruction> instructions;

        public List<Instruction> visit(FunctionTree function) {
            this.instructions = new ArrayList<>();
            
            String functionName = function.name().name().asString();
            if (functionName.equals("main")) {
                this.instructions.add(new Function("_main", List.of()));
                visitor.visit(function.body(), Map.of());
            } else {
                List<Var> parameters = new ArrayList<>();
                for (ParamTree param : function.parameters()) {
                    String paramName = param.name().name().asString();
                    parameters.add(new Var(paramName));
                }
                this.instructions.add(new Function(functionName, parameters));
                visitor.visit(function.body(), Map.of());
            }

            return this.instructions;
        }

        ////// Statements Start //////

        public void visit(StatementTree statement, Map<String, String> data) {
            switch (statement) {
                /* simp (end with `;` ) */
                case AssignmentTree assignmentTree -> {
                    visitor.visit(assignmentTree, data);
                }
                case DeclarationTree declarationTree -> {
                    visitor.visit(declarationTree, data);
                }
                case FunctionCallTree functionCall -> {
                    visitor.visit(functionCall, data);
                }

                /* control  */
                case IfTree ifTree -> {
                    visitor.visit(ifTree, data);
                }
                case ReturnTree returnTree -> {
                    visitor.visit(returnTree, data);
                }
                case WhileTree whileTree -> {
                    visitor.visit(whileTree, data);
                }
                case ForTree forTree -> {
                    visitor.visit(forTree, data);
                }
                case ContinueTree continueTree -> {
                    visitor.visit(continueTree, data);
                }
                case BreakTree breakTree -> {
                    visitor.visit(breakTree, data);
                }


                /* block */
                case BlockTree blockTree -> {
                    visitor.visit(blockTree, data);
                }
                default -> {
                    throw new IllegalArgumentException("Unknown statement type: " + statement.getClass().getName());
                }
            }
        }

        public void visit(ReturnTree returnTree, Map<String, String> data) {
            Val val = visitor.visit(returnTree.expression(), data);
            this.instructions.add(new Return(val));
        }

        public void visit(AssignmentTree assignmentTree, Map<String, String> data) {
            Val rhs = visitor.visit(assignmentTree.expression(), data);
            Var lhs = visitor.visit(assignmentTree.lValue(), data);
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
                    this.instructions.add(new Binary(OperatorType.BIT_AND, lhs, rhs, lhs));
                }
                case ASSIGN_OR -> {
                    this.instructions.add(new Binary(OperatorType.BIT_OR, lhs, rhs, lhs));
                }
                case ASSIGN_XOR -> {
                    this.instructions.add(new Binary(OperatorType.BIT_XOR, lhs, rhs, lhs));
                }
                case ASSIGN_SHIFT_LEFT -> {
                    this.instructions.add(new Binary(OperatorType.SHIFT_LEFT, lhs, rhs, lhs));
                }
                case ASSIGN_SHIFT_RIGHT -> {
                    this.instructions.add(new Binary(OperatorType.SHIFT_RIGHT, lhs, rhs, lhs));
                }
                default -> {
                    throw new IllegalArgumentException("Unknown assignment operator: " + operatorType);
                }
            }
        }

        public Var visit(LValueTree lValueTree, Map<String, String> data) {
            switch (lValueTree) {
                case LValueIdentTree lValueIdentTree -> {
                    return visitor.visit(lValueIdentTree.name(), data);
                }
                default -> throw new IllegalArgumentException("Unknown lvalue type: " + lValueTree.getClass().getName());
            }
        }

        public Var visit(NameTree nameTree, Map<String, String> data) {
            return new Var(nameTree.name().asString());
        }

        public void visit(DeclarationTree declarationTree, Map<String, String> data) {
            // NOTE: "declare" is not an instruction in IR/Asm, we regard it as a assignment
            if (declarationTree.initializer() != null) {
                Val rhs = visitor.visit(declarationTree.initializer(), data);
                Var dst = visitor.visit(declarationTree.name(), data);
                this.instructions.add(new Copy(rhs, dst));
            } else {
                Var dst = visitor.visit(declarationTree.name(), data);
                this.instructions.add(new Copy(new Constant(OptionalLong.of(0)), dst));
            }
        }

        public Val visit(FunctionCallTree functionCallTree, Map<String, String> data) {
            List<Val> args = new ArrayList<>();
            for (ExpressionTree argument : functionCallTree.arguments()) {
                args.add(visitor.visit(argument, data));
            }
            String dst_name = Utils.makeTemp();
            Var dst = new Var(dst_name);
            this.instructions.add(new FunctionCall(functionCallTree.name().name().asString(), args, dst));
            return dst;
        }

        public void visit(IfTree ifTree, Map<String, String> data) {
            if (ifTree.elseStatement() == null) {
                // Only if
                String endLabel = Utils.makeLabel();

                Val condition = visitor.visit(ifTree.condition(), data);
                this.instructions.add(new JumpIfZero(condition, endLabel));
                visitor.visit(ifTree.thenStatement(), Map.of());
                this.instructions.add(new Label(endLabel));
            } else {
                // If-Else
                String elseLabel = Utils.makeLabel();
                String endLabel = Utils.makeLabel();

                Val condition = visitor.visit(ifTree.condition(), data);
                this.instructions.add(new JumpIfZero(condition, elseLabel));
                visitor.visit(ifTree.thenStatement(), Map.of());
                this.instructions.add(new Jump(endLabel));
                this.instructions.add(new Label(elseLabel));
                visitor.visit(ifTree.elseStatement(), Map.of());
                this.instructions.add(new Label(endLabel));
            }
        }

        public void visit(WhileTree whileTree, Map<String, String> data) {
            String continueLabel = Utils.makeLabel();
            String breakLabel = Utils.makeLabel();

            this.instructions.add(new Label(continueLabel));
            Val condition = visitor.visit(whileTree.condition(), data);
            this.instructions.add(new JumpIfZero(condition, breakLabel));
            visitor.visit(whileTree.body(), Map.of(
                "continueLabel", continueLabel,
                "breakLabel", breakLabel
            ));
            this.instructions.add(new Jump(continueLabel));
            this.instructions.add(new Label(breakLabel));
        }

        public void visit(ForTree forTree, Map<String, String> data) {
            String startLabel = Utils.makeLabel();
            String continueLabel = Utils.makeLabel();
            String breakLabel = Utils.makeLabel();

            visitor.visit(forTree.init(), Map.of());
            this.instructions.add(new Label(startLabel));
            Val condition = visitor.visit(forTree.condition(), data);
            this.instructions.add(new JumpIfZero(condition, breakLabel));
            visitor.visit(forTree.body(), Map.of(
                "continueLabel", continueLabel,
                "breakLabel", breakLabel
            ));
            this.instructions.add(new Label(continueLabel));
            visitor.visit(forTree.step(), Map.of());
            this.instructions.add(new Jump(startLabel));
            this.instructions.add(new Label(breakLabel));
        }

        public void visit(ContinueTree continueTree, Map<String, String> data) {
            this.instructions.add(new Jump(data.get("continueLabel")));
        }

        public void visit(BreakTree breakTree, Map<String, String> data) {
            this.instructions.add(new Jump(data.get("breakLabel")));
        }

        public void visit(BlockTree block, Map<String, String> data) {
            for (StatementTree statement : block.statements()) {
                visitor.visit(statement, data);
            }
        }



        
        ////// Expressions Start //////
        
        public Val visit(ExpressionTree expressionTree, Map<String, String> data) {
            switch (expressionTree) {
                case LiteralTree literalTree -> {
                    return new Constant(literalTree.parseValue());
                }
                case UnaryOperationTree unaryOperationTree -> {
                    return visitor.visit(unaryOperationTree, data);
                }
                case BinaryOperationTree binaryOperationTree -> {
                    return visitor.visit(binaryOperationTree, data);
                }
                case IdentExpressionTree identExpressionTree -> {
                    return new Var(identExpressionTree.name().name().asString());
                }
                case ConditionalTree conditionalTree -> {
                    return visitor.visit(conditionalTree, data);
                }
                case FunctionCallTree functionCallTree -> {
                    return visitor.visit(functionCallTree, data);
                }
                default -> throw new IllegalArgumentException("Unknown expression type: " + expressionTree.getClass().getName());
            }
        }

        public Val visit(UnaryOperationTree unaryOperationTree, Map<String, String> data) {
            Val src = visitor.visit(unaryOperationTree.operand(), data);
            String dst_name = Utils.makeTemp();
            Var dst = new Var(dst_name);
            this.instructions.add(new Unary(unaryOperationTree.operatorType(), src, dst));
            return dst;
        }

        public Val visit(BinaryOperationTree binaryOperationTree, Map<String, String> data) {
            OperatorType operatorType = binaryOperationTree.operatorType();

            switch (operatorType) {
                case AND: {
                    Val src1 = visitor.visit(binaryOperationTree.lhs(), data);
                    String falseLabel = Utils.makeLabel();
                    // short circuiting
                    this.instructions.add(new JumpIfZero(src1, falseLabel));
                    Val src2 = visitor.visit(binaryOperationTree.rhs(), data);
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
                    Val src1 = visitor.visit(binaryOperationTree.lhs(), data);
                    String trueLabel = Utils.makeLabel();
                    // short circuiting
                    this.instructions.add(new JumpIfNotZero(src1, trueLabel));
                    Val src2 = visitor.visit(binaryOperationTree.rhs(), data);
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
                    Val src1 = visitor.visit(binaryOperationTree.lhs(), data);
                    Val src2 = visitor.visit(binaryOperationTree.rhs(), data);
                    String dst_name = Utils.makeTemp();
                    Var dst = new Var(dst_name);
                    this.instructions.add(new Binary(binaryOperationTree.operatorType(), src1, src2, dst));
                    return dst;
                }
            }
        }

        public Val visit(ConditionalTree conditionalTree, Map<String, String> data) {
            String falseLabel = Utils.makeLabel();
            String endLabel = Utils.makeLabel();
            Var result = new Var(Utils.makeTemp());

            Val condition = visitor.visit(conditionalTree.condition(), data);
            this.instructions.add(new JumpIfZero(condition, falseLabel));
            Val trueBranch = visitor.visit(conditionalTree.trueBranch(), data);
            this.instructions.add(new Copy(trueBranch, result));
            this.instructions.add(new Jump(endLabel));
            this.instructions.add(new Label(falseLabel));
            Val falseBranch = visitor.visit(conditionalTree.falseBranch(), data);
            this.instructions.add(new Copy(falseBranch, result));
            this.instructions.add(new Label(endLabel));
            return result;
        }
    }
}
