package edu.kit.kastel.vads.compiler.ir;

import edu.kit.kastel.vads.compiler.ir.node.Block;
import edu.kit.kastel.vads.compiler.ir.node.DivNode;
import edu.kit.kastel.vads.compiler.ir.node.ModNode;
import edu.kit.kastel.vads.compiler.ir.node.Node;
import edu.kit.kastel.vads.compiler.ir.node.Phi;
import edu.kit.kastel.vads.compiler.ir.optimize.Optimizer;
import edu.kit.kastel.vads.compiler.ir.util.DebugInfo;
import edu.kit.kastel.vads.compiler.ir.util.DebugInfoHelper;
import edu.kit.kastel.vads.compiler.lexer.Operator.OperatorType;
import edu.kit.kastel.vads.compiler.parser.ast.*;
import edu.kit.kastel.vads.compiler.parser.symbol.Name;
import edu.kit.kastel.vads.compiler.parser.visitor.Visitor;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Optional;
import java.util.function.BinaryOperator;

/// SSA translation as described in
/// [`Simple and Efficient Construction of Static Single Assignment Form`](https://compilers.cs.uni-saarland.de/papers/bbhlmz13cc.pdf).
///
/// This implementation also tracks side effect edges that can be used to avoid reordering of operations that cannot be
/// reordered.
///
/// We recommend to read the paper to better understand the mechanics implemented here.
public class SsaTranslation {
    private final FunctionTree function;
    private final GraphConstructor constructor;

    public SsaTranslation(FunctionTree function, Optimizer optimizer) {
        this.function = function;
        this.constructor = new GraphConstructor(optimizer, function.name().name().asString());
    }

    public IrGraph translate() {
        var visitor = new SsaTranslationVisitor();
        this.function.accept(visitor, this);
        return this.constructor.graph();
    }

    private void writeVariable(Name variable, Block block, Node value) {
        this.constructor.writeVariable(variable, block, value);
    }

    private Node readVariable(Name variable, Block block) {
        return this.constructor.readVariable(variable, block);
    }

    private Block currentBlock() {
        return this.constructor.currentBlock();
    }

    private static class SsaTranslationVisitor implements Visitor<SsaTranslation, Optional<Node>> {

        @SuppressWarnings("OptionalUsedAsFieldOrParameterType")
        private static final Optional<Node> NOT_AN_EXPRESSION = Optional.empty();

        private final Deque<DebugInfo> debugStack = new ArrayDeque<>();

        private void pushSpan(Tree tree) {
            this.debugStack.push(DebugInfoHelper.getDebugInfo());
            DebugInfoHelper.setDebugInfo(new DebugInfo.SourceInfo(tree.span()));
        }

        private void popSpan() {
            DebugInfoHelper.setDebugInfo(this.debugStack.pop());
        }

        @Override
        public Optional<Node> visit(AssignmentTree assignmentTree, SsaTranslation data) {
            pushSpan(assignmentTree);
            BinaryOperator<Node> desugar = switch (assignmentTree.operator().type()) {
                case ASSIGN_MINUS -> data.constructor::newSub;
                case ASSIGN_PLUS -> data.constructor::newAdd;
                case ASSIGN_MUL -> data.constructor::newMul;
                case ASSIGN_DIV -> (lhs, rhs) -> projResultDivMod(data, data.constructor.newDiv(lhs, rhs));
                case ASSIGN_MOD -> (lhs, rhs) -> projResultDivMod(data, data.constructor.newMod(lhs, rhs));
                case ASSIGN_AND -> data.constructor::newBitAnd;
                case ASSIGN_OR -> data.constructor::newBitOr;
                case ASSIGN_XOR -> data.constructor::newBitXor;
                case ASSIGN_SHIFT_LEFT -> data.constructor::newShiftLeft;
                case ASSIGN_SHIFT_RIGHT -> data.constructor::newShiftRight;
                case ASSIGN -> null;
                default ->
                    throw new IllegalArgumentException("not an assignment operator " + assignmentTree.operator());
            };

            switch (assignmentTree.lValue()) {
                case LValueIdentTree(var name) -> {
                    Node rhs = assignmentTree.expression().accept(this, data).orElseThrow();
                    if (desugar != null) {
                        rhs = desugar.apply(data.readVariable(name.name(), data.currentBlock()), rhs);
                    }
                    data.writeVariable(name.name(), data.currentBlock(), rhs);
                }
            }
            popSpan();
            return NOT_AN_EXPRESSION;
        }

        @Override
        public Optional<Node> visit(BinaryOperationTree binaryOperationTree, SsaTranslation data) {
            pushSpan(binaryOperationTree);
            Node lhs = binaryOperationTree.lhs().accept(this, data).orElseThrow();
            Node rhs = binaryOperationTree.rhs().accept(this, data).orElseThrow();
            Node res = switch (binaryOperationTree.operatorType()) {
                case MINUS -> data.constructor.newSub(lhs, rhs);
                case PLUS -> data.constructor.newAdd(lhs, rhs);
                case MUL -> data.constructor.newMul(lhs, rhs);
                case DIV -> projResultDivMod(data, data.constructor.newDiv(lhs, rhs));
                case MOD -> projResultDivMod(data, data.constructor.newMod(lhs, rhs));
                case BIT_AND -> data.constructor.newBitAnd(lhs, rhs);
                case BIT_OR -> data.constructor.newBitOr(lhs, rhs);
                case BIT_XOR -> data.constructor.newBitXor(lhs, rhs);
                case SHIFT_LEFT -> data.constructor.newShiftLeft(lhs, rhs);
                case SHIFT_RIGHT -> data.constructor.newShiftRight(lhs, rhs);
                case AND -> data.constructor.newAnd(lhs, rhs);
                case OR -> data.constructor.newOr(lhs, rhs);
                case LESS -> data.constructor.newLess(lhs, rhs);
                case LESS_EQUAL -> data.constructor.newLessEqual(lhs, rhs);
                case GREATER -> data.constructor.newGreater(lhs, rhs);
                case GREATER_EQUAL -> data.constructor.newGreaterEqual(lhs, rhs);
                case EQUAL -> data.constructor.newEqual(lhs, rhs);
                case NOT_EQUAL -> data.constructor.newNotEqual(lhs, rhs);
                default ->
                    throw new IllegalArgumentException("not a binary expression operator " + binaryOperationTree.operatorType());
            };
            popSpan();
            return Optional.of(res);
        }

        @Override
        public Optional<Node> visit(BlockTree blockTree, SsaTranslation data) {
            pushSpan(blockTree);
            for (StatementTree statement : blockTree.statements()) {
                statement.accept(this, data);
                // skip everything after a return in a block
                if (statement instanceof ReturnTree) {
                    break;
                }
            }
            popSpan();
            return NOT_AN_EXPRESSION;
        }

        @Override
        public Optional<Node> visit(DeclarationTree declarationTree, SsaTranslation data) {
            pushSpan(declarationTree);
            if (declarationTree.initializer() != null) {
                Node rhs = declarationTree.initializer().accept(this, data).orElseThrow();
                data.writeVariable(declarationTree.name().name(), data.currentBlock(), rhs);
            }
            popSpan();
            return NOT_AN_EXPRESSION;
        }

        @Override
        public Optional<Node> visit(FunctionTree functionTree, SsaTranslation data) {
            pushSpan(functionTree);
            Node start = data.constructor.newStart();
            data.constructor.writeCurrentSideEffect(data.constructor.newSideEffectProj(start));
            functionTree.body().accept(this, data);
            popSpan();
            return NOT_AN_EXPRESSION;
        }

        @Override
        public Optional<Node> visit(IdentExpressionTree identExpressionTree, SsaTranslation data) {
            pushSpan(identExpressionTree);
            Node value = data.readVariable(identExpressionTree.name().name(), data.currentBlock());
            popSpan();
            return Optional.of(value);
        }

        @Override
        public Optional<Node> visit(LiteralTree literalTree, SsaTranslation data) {
            pushSpan(literalTree);
            Node node = data.constructor.newConstInt((int) literalTree.parseValue().orElseThrow());
            popSpan();
            return Optional.of(node);
        }

        @Override
        public Optional<Node> visit(LValueIdentTree lValueIdentTree, SsaTranslation data) {
            return NOT_AN_EXPRESSION;
        }

        @Override
        public Optional<Node> visit(NameTree nameTree, SsaTranslation data) {
            return NOT_AN_EXPRESSION;
        }

        @Override
        public Optional<Node> visit(NegateTree negateTree, SsaTranslation data) {
            pushSpan(negateTree);
            Node node = negateTree.expression().accept(this, data).orElseThrow();
            Node res = data.constructor.newSub(data.constructor.newConstInt(0), node);
            popSpan();
            return Optional.of(res);
        }

        @Override
        public Optional<Node> visit(ProgramTree programTree, SsaTranslation data) {
            throw new UnsupportedOperationException();
        }

        @Override
        public Optional<Node> visit(ReturnTree returnTree, SsaTranslation data) {
            pushSpan(returnTree);
            Node node = returnTree.expression().accept(this, data).orElseThrow();
            Node ret = data.constructor.newReturn(node);
            data.constructor.graph().endBlock().addPredecessor(ret);
            popSpan();
            return NOT_AN_EXPRESSION;
        }

        @Override
        public Optional<Node> visit(TypeTree typeTree, SsaTranslation data) {
            throw new UnsupportedOperationException();
        }

        @Override
        public Optional<Node> visit(IfTree ifTree, SsaTranslation data) {
            pushSpan(ifTree);
            
            // 1. 处理条件表达式
            Node condition = ifTree.condition().accept(this, data).orElseThrow();
            
            // 2. 保存当前块，用于后续创建合并块
            Block currentBlock = data.currentBlock();
            
            // 3. 创建 then 分支块
            Block thenBlock = data.constructor.newBlock();
            data.constructor.sealBlock(thenBlock);
            
            // 4. 处理 then 分支
            data.constructor.setCurrentBlock(thenBlock);
            ifTree.thenStatement().accept(this, data);
            Node thenSideEffect = data.constructor.readCurrentSideEffect();
            
            // 5. 创建 else 分支块（如果存在）
            Block elseBlock = null;
            Node elseSideEffect = null;
            if (ifTree.elseStatement() != null) {
                elseBlock = data.constructor.newBlock();
                data.constructor.sealBlock(elseBlock);
                data.constructor.setCurrentBlock(elseBlock);
                ifTree.elseStatement().accept(this, data);
                elseSideEffect = data.constructor.readCurrentSideEffect();
            }
            
            // 6. 创建合并块
            Block mergeBlock = data.constructor.newBlock();
            data.constructor.sealBlock(mergeBlock);
            
            // 7. 创建 if 节点并设置控制流
            Node ifNode = data.constructor.newIf(condition, thenBlock, elseBlock);
            currentBlock.addPredecessor(ifNode);
            
            // 8. 设置分支到合并块
            thenBlock.addPredecessor(mergeBlock);
            if (elseBlock != null) {
                elseBlock.addPredecessor(mergeBlock);
            }
            
            // 9. 处理副作用
            if (elseBlock != null) {
                // 如果有 else 分支，需要创建 phi 节点来合并副作用
                Phi sideEffectPhi = data.constructor.newPhi();
                sideEffectPhi.appendOperand(thenSideEffect);
                sideEffectPhi.appendOperand(elseSideEffect);
                data.constructor.writeCurrentSideEffect(sideEffectPhi);
            } else {
                // 如果没有 else 分支，直接使用 then 分支的副作用
                data.constructor.writeCurrentSideEffect(thenSideEffect);
            }
            
            // 10. 设置当前块为合并块
            data.constructor.setCurrentBlock(mergeBlock);
            
            popSpan();
            return Optional.of(ifNode);
        }

        @Override
        public Optional<Node> visit(UnaryOperationTree unaryOperationTree, SsaTranslation data) {
            pushSpan(unaryOperationTree);
            Node operand = unaryOperationTree.operand().accept(this, data).orElseThrow();
            Node res = switch (unaryOperationTree.operatorType()) {
                case NOT -> data.constructor.newNot(operand);
                case BIT_NOT -> data.constructor.newBitNot(operand);
                case MINUS -> data.constructor.newSub(data.constructor.newConstInt(0), operand);
                default -> throw new IllegalArgumentException("not a unary operator " + unaryOperationTree.operatorType());
            };
            popSpan();
            return Optional.of(res);
        }

        @Override
        public Optional<Node> visit(ConditionalTree conditionalTree, SsaTranslation data) {
            pushSpan(conditionalTree);
            Node condition = conditionalTree.condition().accept(this, data).orElseThrow();
            Node trueBranch = conditionalTree.trueBranch().accept(this, data).orElseThrow();
            Node falseBranch = conditionalTree.falseBranch().accept(this, data).orElseThrow();
            Node res = data.constructor.newPhi(condition, trueBranch, falseBranch);
            popSpan();
            return Optional.of(res);
        }

        @Override
        public Optional<Node> visit(BreakTree breakTree, SsaTranslation data) {
            return NOT_AN_EXPRESSION;
        }

        @Override
        public Optional<Node> visit(ContinueTree continueTree, SsaTranslation data) {
            return NOT_AN_EXPRESSION;
        }

        @Override
        public Optional<Node> visit(ForTree forTree, SsaTranslation data) {
            pushSpan(forTree);
            
            // 1. 保存当前块作为循环入口块
            Block entryBlock = data.currentBlock();
            
            // 2. 处理初始化语句（如果存在）
            if (forTree.init() != null) {
                forTree.init().accept(this, data);
            }
            
            // 3. 创建循环头块
            Block headerBlock = data.constructor.newBlock();
            data.constructor.sealBlock(headerBlock);
            data.constructor.setCurrentBlock(headerBlock);
            
            // 4. 处理循环条件
            Node condition = forTree.condition().accept(this, data).orElseThrow();
            
            // 5. 创建循环体块
            Block bodyBlock = data.constructor.newBlock();
            data.constructor.sealBlock(bodyBlock);
            data.constructor.setCurrentBlock(bodyBlock);
            
            // 6. 处理循环体
            forTree.body().accept(this, data);
            Node bodySideEffect = data.constructor.readCurrentSideEffect();
            
            // 7. 创建步进块（用于处理步进表达式）
            Block stepBlock = data.constructor.newBlock();
            data.constructor.sealBlock(stepBlock);
            data.constructor.setCurrentBlock(stepBlock);
            
            // 8. 处理步进表达式（如果存在）
            if (forTree.step() != null) {
                forTree.step().accept(this, data);
            }
            Node stepSideEffect = data.constructor.readCurrentSideEffect();
            
            // 9. 创建循环出口块
            Block exitBlock = data.constructor.newBlock();
            data.constructor.sealBlock(exitBlock);
            
            // 10. 创建 for 节点并设置控制流
            Node forNode = data.constructor.newFor(condition, bodyBlock);
            
            // 11. 设置控制流关系
            entryBlock.addPredecessor(headerBlock);    // 入口块 -> 头块
            headerBlock.addPredecessor(bodyBlock);     // 头块 -> 循环体
            bodyBlock.addPredecessor(stepBlock);       // 循环体 -> 步进块
            stepBlock.addPredecessor(headerBlock);     // 步进块 -> 头块（循环）
            headerBlock.addPredecessor(exitBlock);     // 头块 -> 出口块（条件为假时）
            
            // 12. 处理副作用
            // 在循环头块中创建 phi 节点来合并入口和步进块的副作用
            Phi sideEffectPhi = data.constructor.newPhi();
            sideEffectPhi.appendOperand(data.constructor.readCurrentSideEffect());  // 入口副作用
            sideEffectPhi.appendOperand(stepSideEffect);  // 步进块副作用
            data.constructor.writeCurrentSideEffect(sideEffectPhi);
            
            // 13. 设置当前块为出口块
            data.constructor.setCurrentBlock(exitBlock);
            
            popSpan();
            return Optional.of(forNode);
        }

        @Override
        public Optional<Node> visit(WhileTree whileTree, SsaTranslation data) {
            pushSpan(whileTree);
            
            // 1. 保存当前块作为循环入口块
            Block entryBlock = data.currentBlock();
            
            // 2. 创建循环头块（用于条件判断）
            Block headerBlock = data.constructor.newBlock();
            data.constructor.sealBlock(headerBlock);
            data.constructor.setCurrentBlock(headerBlock);
            
            // 3. 处理循环条件
            Node condition = whileTree.condition().accept(this, data).orElseThrow();
            
            // 4. 创建循环体块
            Block bodyBlock = data.constructor.newBlock();
            data.constructor.sealBlock(bodyBlock);
            data.constructor.setCurrentBlock(bodyBlock);
            
            // 5. 处理循环体
            whileTree.body().accept(this, data);
            Node bodySideEffect = data.constructor.readCurrentSideEffect();
            
            // 6. 创建循环出口块
            Block exitBlock = data.constructor.newBlock();
            data.constructor.sealBlock(exitBlock);
            
            // 7. 创建 while 节点并设置控制流
            Node whileNode = data.constructor.newWhile(condition, bodyBlock);
            
            // 8. 设置控制流关系
            entryBlock.addPredecessor(headerBlock);  // 入口块 -> 头块
            headerBlock.addPredecessor(bodyBlock);   // 头块 -> 循环体
            bodyBlock.addPredecessor(headerBlock);   // 循环体 -> 头块（循环）
            headerBlock.addPredecessor(exitBlock);   // 头块 -> 出口块（条件为假时）
            
            // 9. 处理副作用
            // 在循环头块中创建 phi 节点来合并入口和循环体的副作用
            Phi sideEffectPhi = data.constructor.newPhi();
            sideEffectPhi.appendOperand(data.constructor.readCurrentSideEffect());  // 入口副作用
            sideEffectPhi.appendOperand(bodySideEffect);  // 循环体副作用
            data.constructor.writeCurrentSideEffect(sideEffectPhi);
            
            // 10. 设置当前块为出口块
            data.constructor.setCurrentBlock(exitBlock);
            
            popSpan();
            return Optional.of(whileNode);
        }

        private Node projResultDivMod(SsaTranslation data, Node divMod) {
            // make sure we actually have a div or a mod, as optimizations could
            // have changed it to something else already
            if (!(divMod instanceof DivNode || divMod instanceof ModNode)) {
                return divMod;
            }
            Node projSideEffect = data.constructor.newSideEffectProj(divMod);
            data.constructor.writeCurrentSideEffect(projSideEffect);
            return data.constructor.newResultProj(divMod);
        }
    }


}
