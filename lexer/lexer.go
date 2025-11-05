package lexer

import (
	"MagicInterpreter/token"
	"strings"
)

// lexer represents the state of the lexical analyzer (lexer)
// Its job is to read through the input string character by character
// and break it into a stream of tokens for the parser to consume
type Lexer struct {
	input        string // the entire source code being tokenized
	position     int    // current position in input (points to current char)
	readPosition int    // current reading position in input (after current char)
	ch           byte   // current char under examination
}

// New creates and returns a new lexer instance for the given input string
// It initializes the lexer and reads the first character so its ready to produce tokens
func New(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar()
	return l
}

// NextToken scans the input and returns the next lexical token.
// it is the core function of the lexer, producing a stream of tokens
// from the raw source code
//
// Behavior:
//   - skips the whitespace characters
//   - checks the current character (l.ch) and matches it against known
//     single-character tokens like '=', '+', ';', '(', ')', '{', '}',etc.
//   - checks for multi-character operators such as "==" and "!=" using peekChar()
//   - for letters, calls readIdentifier() to read the full identifier and uses lookupIdent()
//     to determine if it is a keyword or a user-defined identifier
//   - for digits, calls readNumber() to read the full integer literal
//   - If a match is found, it creates a new token using newToken()
//   - Advances the lexer's position to the next character using readChar()
//   - returns an ILLEGAL token if the character ir unrecognized
//   - returns an EOF token when the end of input is reach (If l.ch is 0(end of input), it returns EOF Token)
//
// Returns:
//
//	-token.Token:a struct containing the Type(TokenType) and literal (string value)
func (l *Lexer) NextToken() token.Token {
	var tok token.Token

	l.skipWhitespace()

	switch l.ch {
	// ASSIGN and EQ
	case '=':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.EQ, Literal: literal}
		} else {
			tok = newToken(token.ASSIGN, l.ch)
		}
	case '+':
		tok = newToken(token.PLUS, l.ch)
	case '-':
		tok = newToken(token.MINUS, l.ch)
	// bang and NOT EQ
	case '!':
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.NOT_EQ, Literal: literal}
		} else {
			tok = newToken(token.BANG, l.ch)
		}
	// comment and slash
	case '/':
		if l.peekChar() == '/' {
			// advance  to second '/'
			l.readChar()
			// skip the entire line after this or EOF
			for l.ch != '\n' && l.ch != 0 {
				l.readChar()
			}
			l.readChar()
			return l.NextToken()
		} else {
			tok = newToken(token.SLASH, l.ch)
		}
	case '*':
		tok = newToken(token.ASTERISK, l.ch)
	case '<':
		tok = newToken(token.LT, l.ch)
	case '>':
		tok = newToken(token.GT, l.ch)
	case ';':
		tok = newToken(token.SEMICOLON, l.ch)
	case '(':
		tok = newToken(token.LPAREN, l.ch)
	case ')':
		tok = newToken(token.RPAREN, l.ch)
	case '{':
		tok = newToken(token.LBRACE, l.ch)
	case '}':
		tok = newToken(token.RBRACE, l.ch)
	case ',':
		tok = newToken(token.COMMA, l.ch)
	case '"':
		tok.Type = token.STRING
		tok.Literal = l.readString()
	// Array
	case '[':
		tok = newToken(token.LBRACKET, l.ch)
	case ']':
		tok = newToken(token.RBRACKET, l.ch)
	case ':':
		tok = newToken(token.COLON, l.ch)
	case 0:
		tok.Literal = ""
		tok.Type = token.EOF
	default:
		if isLetter(l.ch) {
			tok.Literal = l.readIdentifier()
			tok.Type = token.LookupIdent(tok.Literal)
			return tok
			//  adding the float as well
		} else if isDigit(l.ch) {
			literal := l.readNumber()
			tok.Literal = literal
			if strings.Contains(literal, ".") {
				tok.Type = token.FLOAT
			} else {
				tok.Type = token.INT
			}
			return tok
		} else {
			tok = newToken(token.ILLEGAL, l.ch)
		}
	}
	l.readChar()
	return tok
}

// newToken is a helper function that creates a new token struct
//
// Arguments
//   - tokenType: the category/type of the token(e.g., ASSIGN, PLUS, SEMICOLON)
//   - ch the actual character from the input (as a byte)
//
// Returns:
//   - A token struct with the Type and literal fields populated
func newToken(tokenType token.TokenType, ch byte) token.Token {
	return token.Token{Type: tokenType, Literal: string(ch)}
}

// readChar advances the lexer by one character
// it updates the current character (ch), the current position, and the read position
// when the end of input is reached, it sets ch to 0(ASCII - NUL) to signal EOF.
func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[l.readPosition]
	}
	l.position = l.readPosition
	l.readPosition += 1
}

// readIdentifier reads a full identifier starting at the lexer's current position and returns it as string
//
// An identifier consists of one or more valid letters (see isLetter).
// The lexer advances its position until it encounters a non-letter character. at which
// point this function returns the substring representing the identifier.
//
// After calling realIdentifier, the lexer's position will point to the first character that is not par of the identifier.
func (l *Lexer) readIdentifier() string {
	position := l.position
	for isLetter(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position]
}

// isLetter reports whether the given byte is a valid letter for identifiers
// It returns true for:
//   - lowercase letters ('a'..'z')
//   - uppercase letters ('A'..'Z')
//   - underscore ('_')
//
// This is used by the lexer to determine where identifiers start and end
// If we want to add , we can add '!' and '?', and treated as letter like '_'
func isLetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

// skipWhitespace makes the lexer ignore spaces, tabs, newlines, and carriage returns
// It checks the following:
//
//	' '  = space
//	'\t' = tab
//	'\n' = newline
//	'\r' = carriage return
//
// In this MagicInterpreter, whitespace only acts as a separator between tokens
func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		l.readChar()
	}
}

// readNumber reads a sequence of digits starting at the lexer's current position.
//
// The lexer advances its position until it encounters a non-digit character, at which
// point this function returns the substring representing the number.
//
// After calling readNumber, the lexer's position will point to the first character that is not part of the number.

// func (l *Lexer) readNumber() string {
// 	position := l.position
// 	for isDigit(l.ch) {
// 		l.readChar()
// 	}
// 	return l.input[position:l.position]
// }

// reading the float number , we check for dot and read char
func (l *Lexer) readNumber() string {
	position := l.position
	hasDot := false

	for isDigit(l.ch) || (l.ch == '.' && !hasDot) {
		if l.ch == '.' {
			hasDot = true
		}
		l.readChar()
	}
	return l.input[position:l.position]
}

// isDigit reports whether the given byte is a digit ('0'–'9').
//
// This helper is used by the lexer to determine if the current character belongs to a number.
func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

// peekChar returns the next character in the input without advancing the lexer's positions
//
// It looks ahead at l.readPosition and returns the byte at that position.
// If l.readPosition is beyond the end of input, it returns 0 (ASCII NUL) to indicate EOF
//
// This is useful for checking the next character (e.g., for two-character operators like "==")
// without moving the lexer's current position forward.
func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	} else {
		return l.input[l.readPosition]
	}
}

// readString
func (l *Lexer) readString() string {
	position := l.position + 1
	for {
		l.readChar()
		if l.ch == '"' || l.ch == 0 {
			break
		}
	}
	return l.input[position:l.position]
}
