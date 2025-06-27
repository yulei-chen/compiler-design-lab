package edu.kit.kastel.vads.compiler.parser.ast;

public sealed interface ExpressionTree extends Tree permits LiteralTree, UnaryOperationTree, BinaryOperationTree, IdentExpressionTree, ConditionalTree, NegateTree, FunctionCall {
}
