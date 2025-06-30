package edu.kit.kastel.vads.compiler.semantic;

import edu.kit.kastel.vads.compiler.parser.ast.*;
import edu.kit.kastel.vads.compiler.parser.type.BasicType;
import edu.kit.kastel.vads.compiler.parser.visitor.NoOpVisitor;
import edu.kit.kastel.vads.compiler.parser.visitor.Unit;
import org.jspecify.annotations.Nullable;

import java.util.HashMap;
import java.util.Map;

/// Checks type compatibility in assignments and function calls.
class TypeCheckingAnalysis implements NoOpVisitor<TypeCheckingAnalysis.TypeCheckingState> {

    static class TypeCheckingState {
        // Map function names to their return types
        Map<String, BasicType> functionReturnTypes = new HashMap<>();
        // Map variable names to their declared types
        Map<String, BasicType> variableTypes = new HashMap<>();
    }

    @Override
    public Unit visit(FunctionTree functionTree, TypeCheckingState data) {
        // Store function return type
        if (functionTree.returnType().type() instanceof BasicType returnType) {
            data.functionReturnTypes.put(functionTree.name().name().asString(), returnType);
        }
        return NoOpVisitor.super.visit(functionTree, data);
    }

    @Override
    public Unit visit(DeclarationTree declarationTree, TypeCheckingState data) {
        // Store variable type
        if (declarationTree.type().type() instanceof BasicType varType) {
            String varName = declarationTree.name().name().asString();
            data.variableTypes.put(varName, varType);
            
            // Check initializer type if present
            if (declarationTree.initializer() != null) {
                BasicType initType = getExpressionType(declarationTree.initializer(), data);
                if (initType != null && initType != varType) {
                    throw new SemanticException("type mismatch: cannot assign " + initType + " to " + varType + " variable " + varName);
                }
            }
        }
        return NoOpVisitor.super.visit(declarationTree, data);
    }

    @Override
    public Unit visit(AssignmentTree assignmentTree, TypeCheckingState data) {
        switch (assignmentTree.lValue()) {
            case LValueIdentTree(var name) -> {
                String varName = name.name().asString();
                BasicType varType = data.variableTypes.get(varName);
                if (varType != null) {
                    BasicType exprType = getExpressionType(assignmentTree.expression(), data);
                    if (exprType != null && exprType != varType) {
                        throw new SemanticException("type mismatch: cannot assign " + exprType + " to " + varType + " variable " + varName);
                    }
                }
            }
        }
        return NoOpVisitor.super.visit(assignmentTree, data);
    }

    private @Nullable BasicType getExpressionType(ExpressionTree expression, TypeCheckingState data) {
        return switch (expression) {
            case LiteralTree literalTree -> {
                if (literalTree.raw().equals("true") || literalTree.raw().equals("false")) {
                    yield BasicType.BOOL;
                } else {
                    yield BasicType.INT;
                }
            }
            case FunctionCallTree functionCallTree -> {
                String funcName = functionCallTree.name().name().asString();
                BasicType returnType = data.functionReturnTypes.get(funcName);
                if (returnType == null) {
                    throw new SemanticException("function " + funcName + " is not defined");
                }
                yield returnType;
            }
            case IdentExpressionTree identExpressionTree -> {
                String varName = identExpressionTree.name().name().asString();
                BasicType varType = data.variableTypes.get(varName);
                if (varType == null) {
                    throw new SemanticException("variable " + varName + " is not defined");
                }
                yield varType;
            }
            default -> null; // For other expression types, we don't check types yet
        };
    }
} 