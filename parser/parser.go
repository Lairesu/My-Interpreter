package parser

import (
	"fmt"
	"MagicInterpreter/ast"
	"MagicInterpreter/lexer"
	"MagicInterpreter/token"
	"strconv"
)

// -----------------------------
// Constants
// -----------------------------
const (
	_ int = iota
	LOWEST
	EQUALS      // ===
	LESSGREATER // > <
	SUM         // +
	PRODUCT     // *
	PREFIX      // -X pr !X
	CALL        // myFunction(X)

	// array[index]
	INDEX
)

// -----------------------------
// precedence
// -----------------------------
// precedence defines the operator precedence for different token types.
// Higher values indicate higher precedence (tighter binding).
//
// For example:
//   - and /  → PRODUCT precedence (evaluated before + and -)
//   - and -  → SUM precedence
//     <, >     → LESSGREATER precedence
//     ==, !=   → EQUALS precedence
var precedence = map[token.TokenType]int{
	token.EQ:       EQUALS,
	token.NOT_EQ:   EQUALS,
	token.LT:       LESSGREATER,
	token.GT:       LESSGREATER,
	token.PLUS:     SUM,
	token.MINUS:    SUM,
	token.SLASH:    PRODUCT,
	token.ASTERISK: PRODUCT,
	token.LPAREN:   CALL,

	// array[index]
	token.LBRACKET: INDEX,
}

// -----------------------------
// Precedence Helpers
// -----------------------------
// peekPrecedence returns the precedence of the upcoming (peek) token.
// If the next token’s type exists in the precedence map, it returns that value;
// otherwise, it returns LOWEST. This is used to decide whether to
// continue parsing an infix expression based on operator binding strength.
func (p *Parser) peekPrecedence() int {
	if p, ok := precedence[p.peekToken.Type]; ok {
		return p
	}
	return LOWEST
}

// curPrecedence returns the precedence of the current token.
// Similar to peekPrecedence, but checks the token currently being parsed.
// This helps determine how tightly the current operator should bind
// relative to other tokens in the expression.
func (p *Parser) curPrecedence() int {
	if p, ok := precedence[p.curToken.Type]; ok {
		return p
	}
	return LOWEST
}

// -----------------------------
// Function Type Aliases
// -----------------------------
type (
	// prefixParseFn handles parsing expressions that appear in prefix position (e.g., -X or !X).
	prefixParseFn func() ast.Expression

	// infixParseFn handles parsing expressions that appear in infix position (e.g., X + Y).
	infixParseFn func(ast.Expression) ast.Expression
)

// -----------------------------
// Parser Definition
// -----------------------------
//
// Parser converts a stream of tokens from the lexer into an Abstract Syntax Tree (AST).
// It maintains the current and next tokens (lookahead) to guide parsing decisions,
// and uses registered functions to handle different prefix and infix expressions.
//
// Fields:
//   - l: the lexer providing tokens
//   - errors: a list of parsing error messages
//   - curToken, peekToken: track current and upcoming tokens
//   - prefixParseFns, infixParseFns: maps of handler functions for expression parsing
type Parser struct {
	l      *lexer.Lexer
	errors []string

	curToken  token.Token
	peekToken token.Token

	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns  map[token.TokenType]infixParseFn
}

// -----------------------------
// Constructor
// -----------------------------

// New creates a new Parser instance given a lexer and initializes its prefix and infix parse functions
func New(l *lexer.Lexer) *Parser {
	p := &Parser{l: l, errors: []string{}}

	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	p.registerPrefix(token.IDENT, p.parseIdentifier)
	p.registerPrefix(token.INT, p.parseIntegerLiteral)
	p.registerPrefix(token.BANG, p.parsePrefixExpression)
	p.registerPrefix(token.MINUS, p.parsePrefixExpression)

	// register prefixParseFn for token.TRUE and token.FALSE tokens
	p.registerPrefix(token.TRUE, p.parseBoolean)
	p.registerPrefix(token.FALSE, p.parseBoolean)

	// registering prefixParseFn for Grouped Expression
	p.registerPrefix(token.LPAREN, p.parseGroupedExpression)

	// register conditional tokens
	p.registerPrefix(token.IF, p.parseIfExpression)

	// register functionLiteral tokens
	p.registerPrefix(token.FUNCTION, p.parseFunctionLiteral)

	// register string Literal
	p.registerPrefix(token.STRING, p.parseStringLiteral)

	// register Lbracket
	p.registerPrefix(token.LBRACKET, p.parseArrayLiteral)

	// register LBRACE
	p.registerPrefix(token.LBRACE, p.parseHashLiteral)

	// register Float
	p.registerPrefix(token.FLOAT, p.parseFloatLiteral)

	// InfixPrefix
	p.infixParseFns = make(map[token.TokenType]infixParseFn)
	p.registerInfix(token.PLUS, p.parseInfixExpression)
	p.registerInfix(token.MINUS, p.parseInfixExpression)
	p.registerInfix(token.SLASH, p.parseInfixExpression)
	p.registerInfix(token.ASTERISK, p.parseInfixExpression)
	p.registerInfix(token.EQ, p.parseInfixExpression)
	p.registerInfix(token.NOT_EQ, p.parseInfixExpression)
	p.registerInfix(token.LT, p.parseInfixExpression)
	p.registerInfix(token.GT, p.parseInfixExpression)

	// register infix CallExpression
	p.registerInfix(token.LPAREN, p.parseCallExpression)

	// register infix LBRACKET
	p.registerInfix(token.LBRACKET, p.parseIndexExpression)

	// Read two tokens so curToken and peekToken are both set
	p.nextToken()
	p.nextToken()

	return p
}

// -----------------------------
// Token Utilities
// -----------------------------

// nextToken advances both tokens forward by one
func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

// curTokenIs checks if the current token matches the given type.
func (p *Parser) curTokenIs(t token.TokenType) bool {
	return p.curToken.Type == t
}

// peekTokenIs checks if the nex token matches the given type.
func (p *Parser) peekTokenIs(t token.TokenType) bool {
	return p.peekToken.Type == t
}

// expectPeek checks whether the next token is of the expected type.
// If so, it advances the tokens and returns true; otherwise, false.
func (p *Parser) expectPeek(t token.TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	}
	return false
}

// -----------------------------
// Error Handling
// -----------------------------

// Errors returns the list of parser errors
func (p *Parser) Errors() []string {
	return p.errors
}

// peekError adds a formatted error message when the next token type is unexpected
func (p *Parser) peekError(t token.TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead", t, p.peekToken.Type)
	p.errors = append(p.errors, msg)
}

// no prefixParseFnError adds and error for missing prefix parse function
func (p *Parser) noPrefixParseFnError(t token.TokenType) {
	msg := fmt.Sprintf("no prefix parse function for %s found", t)
	p.errors = append(p.errors, msg)
}

// -----------------------------
// Function Registration
// -----------------------------
// Helper methods that add entries to the maps prefixParseFns and infixParseFns
func (p *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

func (p *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

// -----------------------------
// Top-Level Parsing
// -----------------------------

// ParseProgram parses the entire input and returns the root AST node.
// It iterates over tokens until reaching EOF, parsing each statement with parseStatement.
// Non-nil statements are appended to the Program's Statements slice.
// Returns the *ast.Program containing all parsed statements.
func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for p.curToken.Type != token.EOF {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.nextToken()
	}

	return program
}

// -----------------------------
// Statement Parsers
// -----------------------------

func (p *Parser) parseStatement() ast.Statement {
	switch p.curToken.Type {
	case token.LET:
		return p.parseLetStatement()
	case token.RETURN:
		return p.parseReturnStatement()
	default:
		return p.parseExpressionStatement()
	}
}

// parseLetStatement parses a 'let' statement and returns an *ast.LetStatement node.
// It expects an identifier after 'let', then an '=' token, and skips tokens
// until a semicolon is reached. Returns nil if any expectation fails.
func (p *Parser) parseLetStatement() *ast.LetStatement {
	stmt := &ast.LetStatement{Token: p.curToken}

	if !p.expectPeek(token.IDENT) {
		return nil
	}

	stmt.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectPeek(token.ASSIGN) {
		return nil
	}

	p.nextToken()

	stmt.Value = p.parseExpression(LOWEST)

	for !p.curTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

// parseReturnStatement parses 'Return' statement and returns an *ast.ReturnStatement node
// parseReturnStatement parses a 'return' statement and its optional return expression
// until a semicolon is reached. Returns nil if parsing fails.
func (p *Parser) parseReturnStatement() *ast.ReturnStatement {

	stmt := &ast.ReturnStatement{Token: p.curToken}

	p.nextToken()

	// ADD THIS IN NEXT TIME YOU DUMBASS PAST RAI and FUTURE RAI REMEMBER
	stmt.ReturnValue = p.parseExpression(LOWEST)

	for !p.curTokenIs(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

// ParseExpressionStatement
func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	// defer untrace(trace("parseExpressionStatement"))
	stmt := &ast.ExpressionStatement{Token: p.curToken}
	stmt.Expression = p.parseExpression(LOWEST)

	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}
	return stmt
}

// -----------------------------
// Expression Parsers
// -----------------------------

// parseExpression parses an expression starting at the current token,
// using the Pratt parsing technique. It first looks up a prefix parsing
// function for the current token and uses it to construct the initial
// (left-hand) expression. Then, while the next token has higher precedence,
// it looks up the corresponding infix parsing function and parses the
// infix expression, updating the left-hand expression each time.
//
// The `precedence` parameter determines the minimum precedence level
// required to continue parsing infix expressions. Parsing stops when a
// token with lower or equal precedence (or a semicolon) is encountered.
//
// Returns an ast.Expression representing the parsed expression, or nil
// if no suitable prefix function exists for the current token.
func (p *Parser) parseExpression(precedence int) ast.Expression {
	// defer untrace(trace("parseExpression"))
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}
	leftExp := prefix()

	for !p.peekTokenIs(token.SEMICOLON) && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}

		p.nextToken()

		leftExp = infix(leftExp)
	}
	return leftExp
}

// returns *ast.Identifier(which means it returns node type representing identifier) with current token in the TOken Field and the literal value of the token in Value
// does not advance the tokens, it doesn't call nextToken.
func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
}

// parseIntegerLiteral parses integer literals into *ast.IntegerLiteral
func (p *Parser) parseIntegerLiteral() ast.Expression {
	// defer untrace(trace("parseIntegerLiteral"))
	lit := &ast.IntegerLiteral{Token: p.curToken}

	value, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer", p.curToken.Literal)
		p.errors = append(p.errors, msg)
	}

	lit.Value = value

	return lit
}

// parsePrefixExpression builds an AST node(*ast.PrefixExpressions)
// advances our token by calling p.nextToken()
// like !X or -X.
func (p *Parser) parsePrefixExpression() ast.Expression {
	// defer untrace(trace("parsePrefixExpression"))
	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}

	p.nextToken()

	expression.Right = p.parseExpression(PREFIX)

	return expression
}

// parseInfixExpression builds an AST node(*ast.InfixExpressions)
// advances our token by calling p.nextToken()
// like 5 + 5
// takes an argument, an ast.Expression called left.
// constructs an *ast.Infix Expression node with left being in the left field
func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	// defer untrace(trace("parseInfixExpression"))
	expression := &ast.InfixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}
	precedence := p.curPrecedence()
	p.nextToken()
	expression.Right = p.parseExpression(precedence)

	return expression
}

// parseBoolean parses a boolean literal ("true" or "false") and returns
// an *ast.Boolean node representing it.
//
// It uses the parser's current token (`p.curToken`) to populate the node's
// Token field and determines the boolean value based on whether the token type
// is token.TRUE.
//
// Example:
//
//	Input:  "true"
//	Output: &ast.Boolean{Token: {Type: token.TRUE, Literal: "true"}, Value: true}
func (p *Parser) parseBoolean() ast.Expression {
	return &ast.Boolean{Token: p.curToken, Value: p.curTokenIs(token.TRUE)}
}

// parseGroupedExpression parses a parenthesized expression.
// It assumes the current token is '(' and returns the parsed expression.
// Returns nil if the closing ')' is missing.
func (p *Parser) parseGroupedExpression() ast.Expression {
	p.nextToken()

	exp := p.parseExpression(LOWEST)

	if !p.expectPeek(token.RPAREN) {
		return nil
	}
	return exp
}

// parseIfExpression parses an if expression of the form
// `if (condition) { ... }` or `if (condition) { ... } else { ... }`
// It expects the current token to be 'if'. Returns nil if the syntax is invalid.
func (p *Parser) parseIfExpression() ast.Expression {
	expression := &ast.IfExpression{Token: p.curToken}

	if !p.expectPeek(token.LPAREN) {
		return nil
	}

	p.nextToken()
	expression.Condition = p.parseExpression(LOWEST)

	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	if !p.expectPeek(token.LBRACE) {
		return nil
	}

	expression.Consequence = p.parseBlockStatement()

	if p.peekTokenIs(token.ELSE) {
		p.nextToken()

		if !p.expectPeek(token.LBRACE) {
			return nil
		}

		expression.Alternative = p.parseBlockStatement()
	}

	return expression
}

// parseBlockStatement parses a block of statements enclosed by braces `{ ... }`.
// Continues parsing statements until it encounters a '}' or EOF.
// Returns a BlockStatement containing all successfully parsed statements.
func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Token: p.curToken}
	block.Statements = []ast.Statement{}

	p.nextToken()

	for !p.curTokenIs(token.RBRACE) && !p.curTokenIs(token.EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		p.nextToken()
	}
	return block
}

func (p *Parser) parseFunctionLiteral() ast.Expression {
	lit := &ast.FunctionLiteral{Token: p.curToken}

	if !p.expectPeek(token.LPAREN) {
		return nil
	}

	lit.Parameters = p.parseFunctionParameters()

	if !p.expectPeek(token.LBRACE) {
		return nil
	}

	lit.Body = p.parseBlockStatement()

	return lit
}

// helper function to parseFunctionLiteral
// constructs the slice of parameters by repeatedly building identifiers from comma separated list
// makes and early exit if the list is empty and it carefully handles the varying sizes
func (p *Parser) parseFunctionParameters() []*ast.Identifier {
	identifiers := []*ast.Identifier{}

	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		return identifiers
	}

	p.nextToken()

	ident := &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
	identifiers = append(identifiers, ident)

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()
		ident := &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
		identifiers = append(identifiers, ident)
	}

	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	return identifiers
}

// Parsing CallExpression
func (p *Parser) parseCallExpression(function ast.Expression) ast.Expression {
	exp := &ast.CallExpression{Token: p.curToken, Function: function}
	// exp.Arguments = p.ParseCallArguments()
	// signifies the end of the arguments list
	exp.Arguments = p.parseExpressionList(token.RPAREN)
	return exp
}

func (p *Parser) ParseCallArguments() []ast.Expression {
	args := []ast.Expression{}

	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		return args
	}

	p.nextToken()
	args = append(args, p.parseExpression(LOWEST))

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()

		args = append(args, p.parseExpression(LOWEST))
	}
	if !p.expectPeek(token.RPAREN) {
		return nil
	}
	return args
}

// parse String Literal
func (p *Parser) parseStringLiteral() ast.Expression {
	return &ast.StringLiteral{Token: p.curToken, Value: p.curToken.Literal}
}

// parseArrayLiteral
func (p *Parser) parseArrayLiteral() ast.Expression {
	array := &ast.ArrayLiteral{Token: p.curToken}

	array.Elements = p.parseExpressionList(token.RBRACKET)

	return array
}

// parseExpressionList
func (p *Parser) parseExpressionList(end token.TokenType) []ast.Expression {
	list := []ast.Expression{}

	if p.peekTokenIs(end) {
		p.nextToken()
		return list
	}

	p.nextToken()
	list = append(list, p.parseExpression(LOWEST))

	for p.peekTokenIs(token.COMMA) {
		p.nextToken()
		p.nextToken()
		list = append(list, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(end) {
		return nil
	}

	return list
}

// parseIndexExpression
func (p *Parser) parseIndexExpression(left ast.Expression) ast.Expression {
	exp := &ast.IndexExpression{Token: p.curToken, Left: left}

	p.nextToken()
	exp.Index = p.parseExpression(LOWEST)

	if !p.expectPeek(token.RBRACKET) {
		return nil
	}

	return exp
}

// Parse the Hash Literals
func (p *Parser) parseHashLiteral() ast.Expression {
	hash := &ast.HashLiteral{Token: p.curToken}
	hash.Pairs = make(map[ast.Expression]ast.Expression)

	for !p.peekTokenIs(token.RBRACE) {
		p.nextToken()
		key := p.parseExpression(LOWEST)

		if !p.expectPeek(token.COLON) {
			return nil
		}

		p.nextToken()
		value := p.parseExpression(LOWEST)

		hash.Pairs[key] = value
		if !p.peekTokenIs(token.RBRACE) && !p.expectPeek(token.COMMA) {
			return nil
		}
	}

	if !p.expectPeek(token.RBRACE) {
		return nil
	}

	return hash
}


// parse the Float 
func (p *Parser) parseFloatLiteral() ast.Expression {
	lit := &ast.FloatLiteral{Token: p.curToken}

	value, err := strconv.ParseFloat(p.curToken.Literal, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as float", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}
	lit.Value = value
	return lit
}