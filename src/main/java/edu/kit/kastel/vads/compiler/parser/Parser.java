package edu.kit.kastel.vads.compiler.parser;

import edu.kit.kastel.vads.compiler.lexer.Identifier;
import edu.kit.kastel.vads.compiler.lexer.Keyword;
import edu.kit.kastel.vads.compiler.lexer.KeywordType;
import edu.kit.kastel.vads.compiler.lexer.NumberLiteral;
import edu.kit.kastel.vads.compiler.lexer.Operator;
import edu.kit.kastel.vads.compiler.lexer.Operator.OperatorType;
import edu.kit.kastel.vads.compiler.lexer.Separator;
import edu.kit.kastel.vads.compiler.lexer.Separator.SeparatorType;
import edu.kit.kastel.vads.compiler.Span;
import edu.kit.kastel.vads.compiler.lexer.Token;
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
import edu.kit.kastel.vads.compiler.parser.ast.NegateTree;
import edu.kit.kastel.vads.compiler.parser.ast.ParamTree;
import edu.kit.kastel.vads.compiler.parser.ast.ProgramTree;
import edu.kit.kastel.vads.compiler.parser.ast.ReturnTree;
import edu.kit.kastel.vads.compiler.parser.ast.StatementTree;
import edu.kit.kastel.vads.compiler.parser.ast.Tree;
import edu.kit.kastel.vads.compiler.parser.ast.TypeTree;
import edu.kit.kastel.vads.compiler.parser.ast.UnaryOperationTree;
import edu.kit.kastel.vads.compiler.parser.ast.WhileTree;
import edu.kit.kastel.vads.compiler.parser.symbol.Name;
import edu.kit.kastel.vads.compiler.parser.type.BasicType;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/** Recursive descent parser */
public class Parser {
    private final TokenSource tokenSource;

    public Parser(TokenSource tokenSource) {
        this.tokenSource = tokenSource;
    }

    /**
     * program → function*
     */
    public ProgramTree parseProgram() {
        List<FunctionTree> functions = new ArrayList<>();
        while (this.tokenSource.hasMore()) {
            functions.add(parseFunction());
        }
        return new ProgramTree(functions);
    }

    /**
     * function → type ident param-list block
     */
    private FunctionTree parseFunction() {
        TypeTree type = parseType();
        Identifier identifier = this.tokenSource.expectIdentifier();
        List<ParamTree> parameters = parseParameterList();
        BlockTree body = parseBlock();
        return new FunctionTree(
            type,
            name(identifier),
            parameters,
            body
        );
    }

   
    /**
     * param-list -> () | ( param param-list-follow )
     * @return
     */
    private List<ParamTree> parseParameterList() {
        this.tokenSource.expectSeparator(SeparatorType.PAREN_OPEN);
        if (this.tokenSource.peek() instanceof Separator(var type, _) && type == SeparatorType.PAREN_CLOSE) {
            this.tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE);
            return List.of();
        }
        List<ParamTree> parameters = new ArrayList<>();
        parameters.add(parseParam());
        parameters.addAll(parseParameterListFollow());
        this.tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE);
        return parameters;
    }

    /**
     * param-list-follow -> , param param-list-follow
     * @return
     */
    private List<ParamTree> parseParameterListFollow() {
        List<ParamTree> parameters = new ArrayList<>();
        if (this.tokenSource.peek() instanceof Separator(var type, _) && type == SeparatorType.COMMA) {
            this.tokenSource.expectSeparator(SeparatorType.COMMA);
            parameters.add(parseParam());
            parameters.addAll(parseParameterListFollow());
        }
        return parameters;
    }

    /**
     * param -> type ident
     * @return
     */
    private ParamTree parseParam() {
        TypeTree type = parseType();
        Identifier identifier = this.tokenSource.expectIdentifier();
        return new ParamTree(type, name(identifier));
    }
   

    /**
     * block → "{" stmt* "}"
     */
    private BlockTree parseBlock() {
        Separator bodyOpen = this.tokenSource.expectSeparator(SeparatorType.BRACE_OPEN);
        List<StatementTree> statements = new ArrayList<>();
        while (!(this.tokenSource.peek() instanceof Separator sep && sep.type() == SeparatorType.BRACE_CLOSE)) {
            statements.add(parseStatement());
        }
        Separator bodyClose = this.tokenSource.expectSeparator(SeparatorType.BRACE_CLOSE);
        return new BlockTree(statements, bodyOpen.span().merge(bodyClose.span()));
    }

    /**
     * stmt → simp ";" | control | block
     */
    private StatementTree parseStatement() {
        return switch (this.tokenSource.peek()) {
            case Keyword(var type, _) when type == KeywordType.IF -> parseIf();
            case Keyword(var type, _) when type == KeywordType.WHILE -> parseWhile();
            case Keyword(var type, _) when type == KeywordType.FOR -> parseFor();
            case Keyword(var type, _) when type == KeywordType.CONTINUE -> parseContinue();
            case Keyword(var type, _) when type == KeywordType.BREAK -> parseBreak();
            case Keyword(var type, _) when type == KeywordType.RETURN -> parseReturn();
            case Separator(var type, _) when type == SeparatorType.BRACE_OPEN -> parseBlock();
            default -> { 
                StatementTree simple = parseSimple();
                this.tokenSource.expectSeparator(SeparatorType.SEMICOLON);
                yield simple;
            }
        };
    }

    /**
     * simp → lvalue asnop expr | decl | function-call
     */
    private StatementTree parseSimple() {
        if (this.tokenSource.peek().isKeyword(KeywordType.INT) || this.tokenSource.peek().isKeyword(KeywordType.BOOL)) {
            return parseDeclaration();
        }
        LValueIdentTree lValue = parseLValue();
        if (this.tokenSource.peek() instanceof Operator(var type, _) && (type == OperatorType.ASSIGN || type == OperatorType.ASSIGN_DIV || type == OperatorType.ASSIGN_NEGATE || type == OperatorType.ASSIGN_MOD || type == OperatorType.ASSIGN_MUL || type == OperatorType.ASSIGN_PLUS || type == OperatorType.ASSIGN_SHIFT_LEFT || type == OperatorType.ASSIGN_SHIFT_RIGHT || type == OperatorType.ASSIGN_AND || type == OperatorType.ASSIGN_XOR || type == OperatorType.ASSIGN_OR)) {
            Operator assignmentOperator = parseAssignmentOperator();
            ExpressionTree expression = parseExpression();
            return new AssignmentTree(lValue, assignmentOperator, expression);
        } else {
            Identifier identifier = new Identifier(lValue.name().name().asString(), lValue.name().span());
            List<ExpressionTree> argumentList = parseArgumentList();
            return new FunctionCallTree(name(identifier), argumentList);
        }
    }

    /**
     * decl → type ident ("=" expr)?
     */
    private StatementTree parseDeclaration() {
        TypeTree type = parseType();
        Identifier ident = this.tokenSource.expectIdentifier();
        ExpressionTree init = null;
        if (this.tokenSource.peek().isOperator(OperatorType.ASSIGN)) {
            this.tokenSource.expectOperator(OperatorType.ASSIGN);
            init = parseExpression();
        }
        return new DeclarationTree(type, name(ident), init);
    }

    /**
     * type → "int" | "bool"
     */
    private TypeTree parseType() {
        if (this.tokenSource.peek().isKeyword(KeywordType.INT)) {
            this.tokenSource.consume();
            return new TypeTree(BasicType.INT, this.tokenSource.peek().span());
        }
        if (this.tokenSource.peek().isKeyword(KeywordType.BOOL)) {
            this.tokenSource.consume();
            return new TypeTree(BasicType.BOOL, this.tokenSource.peek().span());
        }
        throw new ParseException("expected type but got " + this.tokenSource.peek());
    }

    /**
     * asnop → "=" | "+=" | "-=" | "*=" | "/=" | "%="
     *         | "&=" | "^=" | "|=" | "<<=" | ">>="
     */
    private Operator parseAssignmentOperator() {
        if (this.tokenSource.peek() instanceof Operator op) {
            return switch (op.type()) {
                case ASSIGN, ASSIGN_DIV, ASSIGN_NEGATE, ASSIGN_MOD, ASSIGN_MUL, ASSIGN_PLUS, 
                     ASSIGN_SHIFT_LEFT, ASSIGN_SHIFT_RIGHT, ASSIGN_AND, ASSIGN_XOR, ASSIGN_OR -> {
                    this.tokenSource.consume();
                    yield op;
                }
                default -> throw new ParseException("expected assignment but got " + op.type());
            };
        }
        throw new ParseException("expected assignment but got " + this.tokenSource.peek());
    }

    /**
     * lvalue → ident | "(" lvalue ")"
     */
    private LValueIdentTree parseLValue() {
        if (this.tokenSource.peek().isSeparator(SeparatorType.PAREN_OPEN)) {
            this.tokenSource.expectSeparator(SeparatorType.PAREN_OPEN);
            LValueIdentTree inner = parseLValue();
            this.tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE);
            return inner;
        }
        Identifier identifier = this.tokenSource.expectIdentifier();
        return new LValueIdentTree(name(identifier));
    }

    /**
     * control → "if" "(" expr ")" stmt ("else" stmt)?
     *           | "while" "(" expr ")" stmt
     *           | "for" "(" simp? ";" expr ";" simp? ")" stmt
     *           | "continue" ";"
     *           | "break" ";"
     *           | "return" expr ";"
     */
    // private StatementTree parseControl() {
    //     if (this.tokenSource.peek().isKeyword(KeywordType.IF)) {
    //         return parseIf();
    //     }
    //     if (this.tokenSource.peek().isKeyword(KeywordType.WHILE)) {
    //         return parseWhile();
    //     }
    //     if (this.tokenSource.peek().isKeyword(KeywordType.FOR)) {
    //         return parseFor();
    //     }
    //     if (this.tokenSource.peek().isKeyword(KeywordType.CONTINUE)) {
    //         return parseContinue();
    //     }
    //     if (this.tokenSource.peek().isKeyword(KeywordType.BREAK)) {
    //         return parseBreak();
    //     }
    //     throw new ParseException("expected control statement but got " + this.tokenSource.peek());
    // }

    /**
     * if → "if" "(" expr ")" stmt ("else" stmt)?
     */
    private StatementTree parseIf() {
        Keyword ifKeyword = this.tokenSource.expectKeyword(KeywordType.IF);
        this.tokenSource.expectSeparator(SeparatorType.PAREN_OPEN);
        ExpressionTree condition = parseExpression();
        this.tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE);
        StatementTree thenStatement = parseStatement();
        StatementTree elseStatement = null;
        if (this.tokenSource.peek().isKeyword(KeywordType.ELSE)) {
            this.tokenSource.expectKeyword(KeywordType.ELSE);
            elseStatement = parseStatement();
        }
        return new IfTree(condition, thenStatement, elseStatement, ifKeyword.span());
    }
   
    /**
     * while → "while" "(" expr ")" stmt
     */
    private StatementTree parseWhile() {
        Keyword whileKeyword = this.tokenSource.expectKeyword(KeywordType.WHILE);
        this.tokenSource.expectSeparator(SeparatorType.PAREN_OPEN);
        ExpressionTree condition = parseExpression();
        this.tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE);
        StatementTree body = parseStatement();
        return new WhileTree(condition, body, whileKeyword.span());
    }

    /**
     * for → "for" "(" simp? ";" expr ";" simp? ")" stmt
     */
    private StatementTree parseFor() {
        Keyword forKeyword = this.tokenSource.expectKeyword(KeywordType.FOR);
        this.tokenSource.expectSeparator(SeparatorType.PAREN_OPEN);
        StatementTree init = null;
        if (this.tokenSource.peek() instanceof Separator(var type, _) && (type == SeparatorType.SEMICOLON)) {
            this.tokenSource.consume();
        } else {
            init = parseSimple();
            this.tokenSource.expectSeparator(SeparatorType.SEMICOLON);
        }
        ExpressionTree condition = parseExpression();
        this.tokenSource.expectSeparator(SeparatorType.SEMICOLON);
        StatementTree step = null;
        if (this.tokenSource.peek() instanceof Separator(var type, _) && (type == SeparatorType.SEMICOLON)) {
            this.tokenSource.consume();
        } else {
            step = parseSimple();
        }
        this.tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE);
        StatementTree body = parseStatement();
        return new ForTree(init, condition, step, body, forKeyword.span());
    }

    /**
     * continue → "continue" ";"
     */
    private StatementTree parseContinue() {
        Keyword continueKeyword = this.tokenSource.expectKeyword(KeywordType.CONTINUE);
        this.tokenSource.expectSeparator(SeparatorType.SEMICOLON);
        return new ContinueTree(continueKeyword.span());
    }

    /**
     * break → "break" ";"
     */
    private StatementTree parseBreak() {
        Keyword breakKeyword = this.tokenSource.expectKeyword(KeywordType.BREAK);
        this.tokenSource.expectSeparator(SeparatorType.SEMICOLON);
        return new BreakTree(breakKeyword.span());
    }

    /**
     * return → "return" expr ";"
     */
    private StatementTree parseReturn() {
        Keyword ret = this.tokenSource.expectKeyword(KeywordType.RETURN);
        ExpressionTree expression = parseExpression();
        this.tokenSource.expectSeparator(SeparatorType.SEMICOLON);  
        return new ReturnTree(expression, ret.span().start());
    }

    /**
     * function-call → ident arg-list
     * @return
     */
    private StatementTree parseFunctionCall() {
        Identifier identifier = this.tokenSource.expectIdentifier();
        List<ExpressionTree> argumentList = parseArgumentList();
        return new FunctionCallTree(name(identifier), argumentList);
    }

    /**
     * arg-list → ( ) | ( expr arg-list-follow )
     * @return
     */
    private List<ExpressionTree> parseArgumentList() {
        this.tokenSource.expectSeparator(SeparatorType.PAREN_OPEN);
        List<ExpressionTree> arguments = new ArrayList<>();
        if (this.tokenSource.peek() instanceof Separator(var type, _) && type == SeparatorType.PAREN_CLOSE) {
            this.tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE);
            return List.of();
        }
        arguments.add(parseExpression());
        arguments.addAll(parseArgumentListFollow());
        this.tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE);
        return arguments;
    }

    /**
     * arg-list-follow → , expr arg-list-follow
     * @return
     */
    private List<ExpressionTree> parseArgumentListFollow() {
        List<ExpressionTree> arguments = new ArrayList<>();
        if (this.tokenSource.peek() instanceof Separator(var type, _) && type == SeparatorType.COMMA) {
            this.tokenSource.expectSeparator(SeparatorType.COMMA);
            arguments.add(parseExpression());
            arguments.addAll(parseArgumentListFollow());
        }
        return arguments;
    }

    /**
     * expr → conditional
     */
    private ExpressionTree parseExpression() {
        return parseConditional();
    }

    /**
     * conditional → logor ("?" expr ":" expr)?
     */
    private ExpressionTree parseConditional() {
        ExpressionTree condition = parseLogOr();
        if (this.tokenSource.peek() instanceof Operator(var type, _) && (type == OperatorType.QUESTION)) {
            this.tokenSource.consume();
            ExpressionTree trueBranch = parseExpression();
            this.tokenSource.expectOperator(OperatorType.COLON);
            ExpressionTree falseBranch = parseExpression();
            return new ConditionalTree(condition, trueBranch, falseBranch, condition.span());
        }
        return condition;
    }

    /**
     * logor → logand ("||" logand)*
     */
    private ExpressionTree parseLogOr() {
        ExpressionTree lhs = parseLogAnd();
        while (this.tokenSource.peek() instanceof Operator(var type, _) && (type == OperatorType.OR)) {
            this.tokenSource.consume();
            ExpressionTree rhs = parseLogAnd();
            lhs = new BinaryOperationTree(lhs, rhs, type);
        }
        return lhs;
    }

    /**
     * logand → bitor ("&&" bitor)*
     */
    private ExpressionTree parseLogAnd() {
        ExpressionTree lhs = parseBitOr();
        while (this.tokenSource.peek() instanceof Operator(var type, _) && (type == OperatorType.AND)) {
            this.tokenSource.consume();
            ExpressionTree rhs = parseBitOr();
            lhs = new BinaryOperationTree(lhs, rhs, type);
        }
        return lhs;
    }

    /**
     * bitor → bitxor ("|" bitxor)*
     */
    private ExpressionTree parseBitOr() {
        ExpressionTree lhs = parseBitXor();
        while (this.tokenSource.peek() instanceof Operator(var type, _) && (type == OperatorType.BIT_OR)) {
            this.tokenSource.consume();
            ExpressionTree rhs = parseBitXor();
            lhs = new BinaryOperationTree(lhs, rhs, type);
        }
        return lhs;
    }

    /**
     * bitxor → bitand ("^" bitand)*
     */
    private ExpressionTree parseBitXor() {
        ExpressionTree lhs = parseBitAnd();
        while (this.tokenSource.peek() instanceof Operator(var type, _) && (type == OperatorType.BIT_XOR)) {
            this.tokenSource.consume();
            ExpressionTree rhs = parseBitAnd();
            lhs = new BinaryOperationTree(lhs, rhs, type);
        }
        return lhs;
    }

    /**
     * bitand → equality ("&" equality)*
     */
    private ExpressionTree parseBitAnd() {
        ExpressionTree lhs = parseEquality();
        while (this.tokenSource.peek() instanceof Operator(var type, _) && (type == OperatorType.BIT_AND)) {
            this.tokenSource.consume();
            ExpressionTree rhs = parseEquality();
            lhs = new BinaryOperationTree(lhs, rhs, type);
        }
        return lhs;
    }

    /**
     * equality → comparison (("==" | "!=") comparison)*
     */
    private ExpressionTree parseEquality() {
        ExpressionTree lhs = parseComparison();
        while (this.tokenSource.peek() instanceof Operator(var type, _) && (type == OperatorType.EQUAL || type == OperatorType.NOT_EQUAL)) {
            this.tokenSource.consume();
            ExpressionTree rhs = parseComparison();
            lhs = new BinaryOperationTree(lhs, rhs, type);
        }
        return lhs;
    }

    /**
     * comparison → shift (("<" | "<=" | ">" | ">=") shift)*
     */
    private ExpressionTree parseComparison() {
        ExpressionTree lhs = parseShift();
        while (this.tokenSource.peek() instanceof Operator(var type, _) && (type == OperatorType.LESS || type == OperatorType.LESS_EQUAL || type == OperatorType.GREATER || type == OperatorType.GREATER_EQUAL)) {
            this.tokenSource.consume();
            ExpressionTree rhs = parseShift();
            lhs = new BinaryOperationTree(lhs, rhs, type);
        }
        return lhs;
    }

    /**
     * shift → term (("<<" | ">>") term)*
     */
    private ExpressionTree parseShift() {
        ExpressionTree lhs = parseTerm();
        while (this.tokenSource.peek() instanceof Operator(var type, _)&& (type == OperatorType.SHIFT_LEFT || type == OperatorType.SHIFT_RIGHT)) {
            this.tokenSource.consume();
            ExpressionTree rhs = parseTerm();
            lhs = new BinaryOperationTree(lhs, rhs, type);
        }
        return lhs;
    }

    /**
     * term → factor (("+" | "-") factor)*
     */
    private ExpressionTree parseTerm() {
        ExpressionTree lhs = parseFactor();
        while (this.tokenSource.peek() instanceof Operator(var type, _) && (type == OperatorType.PLUS || type == OperatorType.NEGATE)) {
            this.tokenSource.consume();
            ExpressionTree rhs = parseFactor();
            lhs = new BinaryOperationTree(lhs, rhs, type);
        }
        return lhs;
    }

    /**
     * factor → unary (("*" | "/" | "%") unary)*
     */
    private ExpressionTree parseFactor() {
        ExpressionTree lhs = parseUnary();
        while (this.tokenSource.peek() instanceof Operator(var type, _) && (type == OperatorType.MUL || type == OperatorType.DIV || type == OperatorType.MOD)) {
            this.tokenSource.consume();
            ExpressionTree rhs = parseUnary();
            lhs = new BinaryOperationTree(lhs, rhs, type);
        }
        return lhs;
    }


    /**
     * unary → ("!" | "~" | "-") unary
     *        | primary
     */
    private ExpressionTree parseUnary() {
        return switch (this.tokenSource.peek()) {
            case Operator(var type, _) when type == OperatorType.NOT || type == OperatorType.COMPLEMENT || type == OperatorType.NEGATE -> {
                Token token = this.tokenSource.consume();
                if (!(token instanceof Operator operator)) {
                    throw new ParseException("expected operator but got " + token);
                }
                yield new UnaryOperationTree(parseUnary(), operator.type(), operator.span());
            }
            default -> parsePrimary();
        };
    }

    /**
     * primary → intconst
     *          | "true" | "false"
     *          | ident
     *          | "(" expr ")"
     *          | function-call
     */
    private ExpressionTree parsePrimary() {
        return switch (this.tokenSource.peek()) {
            case Identifier ident -> {
                this.tokenSource.consume();
                if (this.tokenSource.peek() instanceof Separator(var type, _) && type == SeparatorType.PAREN_OPEN) {
                    List<ExpressionTree> argumentList = parseArgumentList();
                    yield new FunctionCallTree(name(ident), argumentList);
                }
                yield new IdentExpressionTree(name(ident));
            }
            case NumberLiteral(String value, int base, Span span) -> {
                this.tokenSource.consume();
                yield new LiteralTree(value, span, Optional.of(base));
            }
            case Keyword(var type, _) when type == KeywordType.TRUE || type == KeywordType.FALSE -> {
                Span span = this.tokenSource.consume().span();
                yield new LiteralTree(type.toString(), span, Optional.empty());
            }
            case Separator(var type, _) when type == SeparatorType.PAREN_OPEN -> {
                this.tokenSource.consume();
                ExpressionTree expression = parseExpression();
                this.tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE);
                yield expression;
            }
            case Token t -> throw new ParseException("invalid primary " + t);
        };
    }

    private static NameTree name(Identifier ident) {
        return new NameTree(Name.forIdentifier(ident), ident.span());
    }
}
