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
import edu.kit.kastel.vads.compiler.parser.ast.FunctionTree;
import edu.kit.kastel.vads.compiler.parser.ast.IdentExpressionTree;
import edu.kit.kastel.vads.compiler.parser.ast.IfTree;
import edu.kit.kastel.vads.compiler.parser.ast.LValueIdentTree;
import edu.kit.kastel.vads.compiler.parser.ast.LValueTree;
import edu.kit.kastel.vads.compiler.parser.ast.LiteralTree;
import edu.kit.kastel.vads.compiler.parser.ast.NameTree;
import edu.kit.kastel.vads.compiler.parser.ast.NegateTree;
import edu.kit.kastel.vads.compiler.parser.ast.ProgramTree;
import edu.kit.kastel.vads.compiler.parser.ast.ReturnTree;
import edu.kit.kastel.vads.compiler.parser.ast.StatementTree;
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
     * program → function
     */
    public ProgramTree parseProgram() {
        ProgramTree programTree = new ProgramTree(List.of(parseFunction()));
        if (this.tokenSource.hasMore()) {
            throw new ParseException("expected end of input but got " + this.tokenSource.peek());
        }
        return programTree;
    }

    /**
     * function → type ident "(" ")" block
     */
    private FunctionTree parseFunction() {
        Keyword returnType = this.tokenSource.expectKeyword(KeywordType.INT);
        Identifier identifier = this.tokenSource.expectIdentifier();
        this.tokenSource.expectSeparator(SeparatorType.PAREN_OPEN);
        this.tokenSource.expectSeparator(SeparatorType.PAREN_CLOSE);
        BlockTree body = parseBlock();
        return new FunctionTree(
            new TypeTree(BasicType.INT, returnType.span()),
            name(identifier),
            body
        );
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
     * simp → lvalue asnop expr | decl
     */
    private StatementTree parseSimple() {
        if (this.tokenSource.peek().isKeyword(KeywordType.INT) || this.tokenSource.peek().isKeyword(KeywordType.BOOL)) {
            return parseDeclaration();
        }
        LValueTree lValue = parseLValue();
        Operator assignmentOperator = parseAssignmentOperator();
        ExpressionTree expression = parseExpression();
        return new AssignmentTree(lValue, assignmentOperator, expression);
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
                case ASSIGN, ASSIGN_DIV, ASSIGN_MINUS, ASSIGN_MOD, ASSIGN_MUL, ASSIGN_PLUS, ASSIGN_SHIFT_LEFT, ASSIGN_SHIFT_RIGHT -> {
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
    private LValueTree parseLValue() {
        if (this.tokenSource.peek().isSeparator(SeparatorType.PAREN_OPEN)) {
            this.tokenSource.expectSeparator(SeparatorType.PAREN_OPEN);
            LValueTree inner = parseLValue();
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
        while (this.tokenSource.peek() instanceof Operator(var type, _) && (type == OperatorType.MUL || type == OperatorType.MINUS)) {
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
            case Operator(var type, _) when type == OperatorType.NOT || type == OperatorType.BIT_NOT || type == OperatorType.UNARY_MINUS -> {
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
     */
    private ExpressionTree parsePrimary() {
        return switch (this.tokenSource.peek()) {
            case Identifier ident -> {
                this.tokenSource.consume();
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
