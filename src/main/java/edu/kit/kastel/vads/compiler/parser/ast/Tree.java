package edu.kit.kastel.vads.compiler.parser.ast;

import edu.kit.kastel.vads.compiler.Span;
import edu.kit.kastel.vads.compiler.parser.visitor.Visitor;

public sealed interface Tree permits ExpressionTree, FunctionTree, LValueTree, NameTree, ProgramTree, StatementTree, TypeTree, ParamTree {

    Span span();

    <T, R> R accept(Visitor<T, R> visitor, T data);
}
