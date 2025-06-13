package edu.kit.kastel.vads.compiler.semantic;

import edu.kit.kastel.vads.compiler.parser.ast.IfTree;
import edu.kit.kastel.vads.compiler.parser.ast.LiteralTree;
import edu.kit.kastel.vads.compiler.parser.ast.UnaryOperationTree;
import edu.kit.kastel.vads.compiler.parser.ast.ConditionalTree;
import edu.kit.kastel.vads.compiler.parser.ast.BreakTree;
import edu.kit.kastel.vads.compiler.parser.ast.ContinueTree;
import edu.kit.kastel.vads.compiler.parser.ast.ForTree;
import edu.kit.kastel.vads.compiler.parser.ast.WhileTree;
import edu.kit.kastel.vads.compiler.parser.visitor.NoOpVisitor;
import edu.kit.kastel.vads.compiler.parser.visitor.Unit;

public class IntegerLiteralRangeAnalysis implements NoOpVisitor<Namespace<Void>> {

    @Override
    public Unit visit(LiteralTree literalTree, Namespace<Void> data) {
        literalTree.parseValue()
          .orElseThrow(
              () -> new SemanticException("invalid integer literal " + literalTree.raw())
          );
        return NoOpVisitor.super.visit(literalTree, data);
    }

    @Override
    public Unit visit(UnaryOperationTree unaryOperationTree, Namespace<Void> data) {
        return NoOpVisitor.super.visit(unaryOperationTree, data);
    }

    @Override
    public Unit visit(ConditionalTree conditionalTree, Namespace<Void> data) {
        return NoOpVisitor.super.visit(conditionalTree, data);
    }

    @Override
    public Unit visit(IfTree ifTree, Namespace<Void> data) {
        return Unit.INSTANCE;
    }

    @Override
    public Unit visit(BreakTree breakTree, Namespace<Void> data) {
        return NoOpVisitor.super.visit(breakTree, data);
    }

    @Override
    public Unit visit(ContinueTree continueTree, Namespace<Void> data) {
        return NoOpVisitor.super.visit(continueTree, data);
    }

    @Override
    public Unit visit(ForTree forTree, Namespace<Void> data) {
        return NoOpVisitor.super.visit(forTree, data);
    }

    @Override
    public Unit visit(WhileTree whileTree, Namespace<Void> data) {
        return NoOpVisitor.super.visit(whileTree, data);
    }
}