package edu.kit.kastel.vads.compiler.parser.ast;

public sealed interface StatementTree extends Tree permits 
/* simp (end with `;` ) */
AssignmentTree, 
DeclarationTree, 
FunctionCall,

/* control  */
IfTree, 
WhileTree,
ForTree,
ContinueTree,
BreakTree,
ReturnTree, 

/* block */
BlockTree
{}
