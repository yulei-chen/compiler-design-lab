package edu.kit.kastel.vads.compiler.semantic;

import java.util.Map;

import edu.kit.kastel.vads.compiler.parser.ast.FunctionTree;
import edu.kit.kastel.vads.compiler.parser.ast.NameTree;
import edu.kit.kastel.vads.compiler.parser.ast.ParamTree;
import edu.kit.kastel.vads.compiler.parser.symbol.IdentName;
import edu.kit.kastel.vads.compiler.parser.visitor.NoOpVisitor;
import edu.kit.kastel.vads.compiler.parser.visitor.Unit;
import edu.kit.kastel.vads.compiler.semantic.utils.Utils;


/**
 * Function-scope parameter renaming.
 */
public class VariableRename implements NoOpVisitor<Map<String, String>> {
    @Override
    public Unit visit(FunctionTree functionTree, Map<String, String> data) {
        data.clear();
        return NoOpVisitor.super.visit(functionTree, data);
    }

    @Override
    public Unit visit(ParamTree paramTree, Map<String, String> data) {
        String variableName = paramTree.name().name().asString();
        String newName = Utils.makeVariable(variableName);
        data.put(variableName, newName);
        ((IdentName) paramTree.name().name()).rename(newName);
        return NoOpVisitor.super.visit(paramTree, data);
    }

    @Override
    public Unit visit(NameTree nameTree, Map<String, String> data) {
        if (nameTree.name() instanceof IdentName) {
            String variableName = nameTree.name().asString();
            if (data.containsKey(variableName)) {
                String newName = data.get(variableName);    
                ((IdentName) nameTree.name()).rename(newName);
            }
        }
        return NoOpVisitor.super.visit(nameTree, data);
    }
}
