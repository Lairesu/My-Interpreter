package ast

import (
	"bytes"
	"MagicInterpreter/token"
	"strings"
)

// ======================================================
// NODE INTERFACES
// ======================================================

// Node represents a node in the abstract syntax tree.
// All AST nodes implement the Node interface.
type Node interface {
	// TokenLiteral returns the literal value of the token associated with the node.
	// This is useful for debugging and inspecting the AST.
	TokenLiteral() string
	String() string
}

// Statement represents a statement node in the AST.
// Statements perform actions but do not produce values.
// All statement types must implement the StatementNode marker method.
type Statement interface {
	Node
	StatementNode()
}

// Expression represents an expression node in the AST.
// Expressions produce values and can be evaluated.
// All expression types must implement the expressionNode marker method.
type Expression interface {
	Node
	ExpressionNode()
}

// ======================================================
// PROGRAM NODE
// ======================================================

// Implementation of Node.
// Program Node is going to be the root of every AST our parser produces.
// Implements the Statement interface.
// Holds all top-level statements from the source code in Statements []Statement.
// When the parser finishes, we get a program node that contains every parsed statement.
type Program struct {
	Statements []Statement
}

// TokenLiteral() is a helper/debugging utility, not part of core parsing logic.
// Returns the literal string of the token that started the node.
// Program node doesn't have a real token of its own (just a container).
// Returns the first statement's token literal if there is one, or "" otherwise.
func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	}
	return ""
}

// String func creates a buffer and writes the return value of each statement's String() method.
func (p *Program) String() string {
	var out bytes.Buffer

	for _, s := range p.Statements {
		out.WriteString(s.String())
	}
	return out.String()
}

// ======================================================
// LET STATEMENT
// ======================================================

// LetStatement represents a variable binding of the form:
//
//	let <identifier> = <expression>
//
// It implements the Statement interface.
// Fields:
//
//	Name  - holds the identifier of the binding
//	Value - holds the expression that produces the value
type LetStatement struct {
	Token token.Token
	Name  *Identifier
	Value Expression
}

// Statement is a marker method to satisfy the Statement interface.
func (ls *LetStatement) StatementNode() {}

// TokenLiteral returns the literal value of the 'let' token.
func (ls *LetStatement) TokenLiteral() string { return ls.Token.Literal }

// String reconstructs the let statement as source code.
// Example: "let x = 5;"
func (ls *LetStatement) String() string {
	var out bytes.Buffer

	out.WriteString(ls.TokenLiteral() + " ")
	if ls.Name != nil {
		out.WriteString(ls.Name.String())
	}
	out.WriteString(" = ")

	if ls.Value != nil {
		out.WriteString(ls.Value.String())
	}

	out.WriteString(";")
	return out.String()
}

// ======================================================
// IDENTIFIER
// ======================================================

// To hold the identifier of the binding, we have Identifier struct type.
// It implements the Expression interface.
type Identifier struct {
	Token token.Token // the token.IDENT token
	Value string      // the name of the variable
}

// Expression is a marker method to satisfy the Expression interface.
func (i *Identifier) ExpressionNode() {}

// TokenLiteral returns the literal value of the identifier token.
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }

// String returns the identifier's name itself (e.g., "x").
func (i *Identifier) String() string { return i.Value }

// ======================================================
// RETURN STATEMENT
// ======================================================

// ReturnStatement represents:
//
//	return <expression>;
//
// It also implements the Statement interface.
type ReturnStatement struct {
	Token       token.Token // the 'return' token
	ReturnValue Expression
}

// Statement is a marker method to satisfy the Statement interface.
func (rs *ReturnStatement) StatementNode() {}

// TokenLiteral returns the literal value of the 'return' token.
func (rs *ReturnStatement) TokenLiteral() string { return rs.Token.Literal }

// String reconstructs the return statement as source code.
// Example: "return 5;"
func (rs *ReturnStatement) String() string {
	var out bytes.Buffer

	out.WriteString(rs.TokenLiteral() + " ")
	if rs.ReturnValue != nil {
		out.WriteString(rs.ReturnValue.String())
	}
	out.WriteString(";")

	return out.String()
}

// ======================================================
// EXPRESSION STATEMENT
// ======================================================

// ExpressionStatement represents:
// ast.Expression fulfills the ast.Statement interface,
// which means we could add it to the Statements slice of ast.Program.
type ExpressionStatement struct {
	Token      token.Token
	Expression Expression
}

// Statement is a marker method to satisfy the Statement interface.
func (es *ExpressionStatement) StatementNode() {}

// TokenLiteral returns the literal value of the token.
func (es *ExpressionStatement) TokenLiteral() string { return es.Token.Literal }

// String reconstructs the expression statement as source code.
// Returns the expression string or empty string if nil.
func (es *ExpressionStatement) String() string {
	if es.Expression != nil {
		return es.Expression.String()
	}
	return ""
}

// *ast.IntegralLiteral fulfills the ast.Expression interface, just like *ast.Identifier does
// but unlike the *ast.Identifier the value is int64 and not string
type IntegerLiteral struct {
	Token token.Token
	Value int64
}

func (il *IntegerLiteral) ExpressionNode()      {}
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }
func (il *IntegerLiteral) String() string       { return il.Token.Literal }

// PrefixExpression has two fields
//
//	Operator - string that's going to contain "-" or "!"
//	Right 	 - contains the expression in right, allows us to see which operands belong to which operator
type PrefixExpression struct {
	Token    token.Token
	Operator string
	Right    Expression
}

func (pe *PrefixExpression) ExpressionNode()      {}
func (pe *PrefixExpression) TokenLiteral() string { return pe.Token.Literal }
func (pe *PrefixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(pe.Operator)
	out.WriteString(pe.Right.String())
	out.WriteString(")")

	return out.String()
}

// InfixExpression represents an infix operation in the AST.
// It consists of a left-hand expression, an operator, and a right-hand expression.
// For example, in the expression "a + b",
// 'a' is the Left, '+' is the Operator, and 'b' is the Right.
type InfixExpression struct {
	Token    token.Token // The operator token (e.g., '+', '-', '*', '/')
	Left     Expression  // Expression on the left side of the operator
	Operator string      // The operator symbol itself (e.g., "+")
	Right    Expression  // Expression on the right side of the operator
}

// ExpressionNode marks InfixExpression as an expression node.
func (ie *InfixExpression) ExpressionNode() {}

// TokenLiteral returns the literal value of the operator token.
func (ie *InfixExpression) TokenLiteral() string { return ie.Token.Literal }

// String returns a string representation of the infix expression.
// It wraps the expression in parentheses for clarity.
func (ie *InfixExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString(" " + ie.Operator + " ")
	out.WriteString(ie.Right.String())
	out.WriteString(")")
	return out.String()
}

// Boolean represents a boolean literal node in the AST (e.g., "true" or "false").
// It holds the token (from the lexer) and the boolean value.
type Boolean struct {
	Token token.Token // The 'true' or 'false' token.
	Value bool        // The corresponding boolean value.
}

func (b *Boolean) ExpressionNode()      {}
func (b *Boolean) TokenLiteral() string { return b.Token.Literal }
func (b *Boolean) String() string       { return b.Token.Literal }

// IfExpression represents an if-else expression in the AST.
//
// It fulfills the ast.Expression interface and contains:
//   - Token:       The 'if' token.
//   - Condition:   The expression to be evaluated.
//   - Consequence: The block of statements executed if the condition is true.
//   - Alternative: The block executed if the condition is false (optional).
//
// Both Consequence and Alternative reference a BlockStatement,
// which is just a series of statements.
type IfExpression struct {
	Token       token.Token     // The 'if' token.
	Condition   Expression      // The evaluated condition.
	Consequence *BlockStatement // Executed if the condition is true.
	Alternative *BlockStatement // Executed if the condition is false (optional).
}

func (ie *IfExpression) ExpressionNode()      {}
func (ie *IfExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *IfExpression) String() string {
	var out bytes.Buffer

	out.WriteString("if")
	out.WriteString(ie.Condition.String())
	out.WriteString(" ")
	out.WriteString(ie.Consequence.String())

	if ie.Alternative != nil {
		out.WriteString("else ")
		out.WriteString(ie.Alternative.String())
	}

	return out.String()
}

// BlockStatement represents a block of statements enclosed by braces `{}`.
// It appears in constructs like function bodies, if-consequence blocks, and loops.
//
// Example:
//
//	{
//	    x = 5;
//	    y = x + 2;
//	}
type BlockStatement struct {
	Token      token.Token // The '{' token that begins the block.
	Statements []Statement // The list of statements inside the block.
}

// StatementNode marks BlockStatement as implementing the Statement interface.
func (bs *BlockStatement) StatementNode() {}

// TokenLiteral returns the literal value of the block's starting token ("{").
func (bs *BlockStatement) TokenLiteral() string { return bs.Token.Literal }

// String returns a stringified representation of the block's contents.
// It concatenates the string representations of all statements within the block.
func (bs *BlockStatement) String() string {
	var out bytes.Buffer

	for _, s := range bs.Statements {
		out.WriteString(s.String())
	}

	return out.String()
}

// ==============================
// Function Literal
// ==============================
// FunctionLiteral represents a function definition in the AST.
// It includes the function keyword (`fn`), a list of parameters, and a body block.
//
// Example:
//
//	fn(x, y) { return x + y; }
type FunctionLiteral struct {
	Token      token.Token     // The 'fn' token.
	Parameters []*Identifier   // The function's parameter identifiers.
	Body       *BlockStatement // The function body block.
}

// ExpressionNode marks FunctionLiteral as implementing the Expression interface.
func (fl *FunctionLiteral) ExpressionNode() {}

// TokenLiteral returns the literal value of the function's starting token ("fn").
func (fl *FunctionLiteral) TokenLiteral() string { return fl.Token.Literal }

// String returns a stringified version of the function definition.
// For example: "fn(x, y) { return x + y; }"
func (fl *FunctionLiteral) String() string {
	var out bytes.Buffer

	params := []string{}
	for _, p := range fl.Parameters {
		params = append(params, p.String())
	}

	out.WriteString(fl.TokenLiteral())
	out.WriteString("(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(") ")
	out.WriteString(fl.Body.String())

	return out.String()
}

// ==============================
// CallExpression
// ==============================
type CallExpression struct {
	Token     token.Token // The '(' token
	Function  Expression  // Identifier or FunctionLiteral
	Arguments []Expression
}

func (ce *CallExpression) ExpressionNode()      {}
func (ce *CallExpression) TokenLiteral() string { return ce.Token.Literal }
func (ce *CallExpression) String() string {
	var out bytes.Buffer
	args := []string{}
	for _, a := range ce.Arguments {
		args = append(args, a.String())
	}

	out.WriteString(ce.Function.String())
	out.WriteString("(")
	out.WriteString(strings.Join(args, ", "))
	out.WriteString(")")
	return out.String()
}

// ==============================
// String Literal
// ==============================
type StringLiteral struct {
	Token token.Token
	Value string
}

func (sl *StringLiteral) ExpressionNode()      {}
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Literal }
func (sl *StringLiteral) String() string       { return sl.Token.Literal }

// ==============================
// ArrayLiteral
// ==============================
type ArrayLiteral struct {
	Token    token.Token
	Elements []Expression
}

func (al *ArrayLiteral) ExpressionNode()      {}
func (al *ArrayLiteral) TokenLiteral() string { return al.Token.Literal }
func (al *ArrayLiteral) String() string {
	var out bytes.Buffer

	elements := []string{}
	for _, el := range al.Elements {
		elements = append(elements, el.String())
	}

	out.WriteString("[")
	out.WriteString(strings.Join(elements, ", "))
	out.WriteString("]")

	return out.String()
}

// ==============================
// IndexExpression
// ==============================
// <expression>[<expression>]
// left and index are Expression
// left is the object that's being accessed and can be any type
// syntactically, it does not make a difference
// semantically it has to produce an integer

type IndexExpression struct {
	token.Token
	Left  Expression
	Index Expression
}

func (ie *IndexExpression) ExpressionNode()      {}
func (ie *IndexExpression) TokenLiteral() string { return ie.Token.Literal }
func (ie *IndexExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString("[")
	out.WriteString(ie.Index.String())
	out.WriteString("])")
	return out.String()
}

// ==============================
// HasHLiteral
// ==============================
type HashLiteral struct {
	Token token.Token
	Pairs map[Expression]Expression
}

func (hl *HashLiteral) ExpressionNode()      {}
func (hl *HashLiteral) TokenLiteral() string { return hl.Token.Literal }
func (hl *HashLiteral) String() string {
	var out bytes.Buffer

	pairs := []string{}
	for key, value := range hl.Pairs {
		pairs = append(pairs, key.String()+":"+value.String())
	}

	out.WriteString("{")
	out.WriteString(strings.Join(pairs, ", "))
	out.WriteString("}")

	return out.String()
}

// ==========================
//	FloatLiteral
// ==========================

// for float number
type FloatLiteral struct {
	Token token.Token
	Value float64
}

func (fl *FloatLiteral) ExpressionNode() {}
func (fl *FloatLiteral) TokenLiteral() string { return fl.Token.Literal }
func (fl *FloatLiteral) String() string { return fl.Token.Literal }