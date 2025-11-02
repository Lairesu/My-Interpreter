// Package token defines the core structure and constants for lexical tokens in the interpreter
// Tokens are teh smallest meaningful units in the source code
// such as keywords, operators, identifiers, literals and delimiters
package token

// TokenType is just a custom type based on string.
// TokenType represents the type of category of a token
// It is defined as a string for readability and easy comparison.
type TokenType string

// Predefined token types used by the lexer.
// these constants classify the different kinds of tokens that can appear
// in the source code.
const (
	// special tokens
	ILLEGAL = "ILLEGAL"	// Unknown or invalid token
	EOF		= "EOF"		// End-of-file marker

	// Identifiers + literals
	IDENT ="IDENT"	// User-defined identifiers (variables, function names, etc.)
	INT   = "INT"  // Integer literals (e.g., 12345)
	FLOAT = "FLOAT"
	
	// Operators
	ASSIGN	 = "="
	PLUS	 = "+"
	MINUS 	 = "-"
	BANG	 = "!"
	ASTERISK = "*"
	SLASH	 = "/"
	
	LT		 = "<"
	GT		 = ">"

	EQ		 = "=="
	NOT_EQ	 = "!="

	// Delimiters
	COMMA 	  = ","
	SEMICOLON = ";"

	LPAREN = "("
	RPAREN = ")"
	LBRACE = "{"
	RBRACE = "}"

	// keywords
	FUNCTION = "FUNCTION" // 'fn'
	LET 	 = "LET"	  // 'let'
	TRUE 	 = "TRUE"   
	FALSE    = "FALSE"
	IF		 = "IF"
	ELSE     = "ELSE"
	RETURN   = "RETURN"


	// strings
	STRING =  "STRING"

	// Array
	LBRACKET = "["
	RBRACKET = "]"

	// For HASH
	COLON = ":"
)

// Token represents a  single lexical token extracted from the source code.
//
// fields:
//	  - Type: The category/Type of the token(operator, keyword, literal, etc.)
//	  - Literal: The exact string value from the source code that this token represents
type Token struct {
	Type TokenType
	Literal string
}

// keywords maps reserved words to their TokenType.
// Used to distinguish language keywords from user defined identifiers
var keywords = map[string]TokenType{
	"fn"	: FUNCTION,
	"let"	: LET,
	"true"	: TRUE,
	"false"	: FALSE,
	"if"	: IF,
	"else"	: ELSE,
	"return": RETURN,
}

// LookupIdent returns the TokenType for a given identifier
// If the identifier is a keyword, the corresponding TokenType is returned
// Otherwise, IDENT is returned for user-defined identifiers
func LookupIdent(ident string) TokenType {
	if tok,  ok := keywords[ident];  ok{
		return tok
	}
	return IDENT
}