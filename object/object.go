package object

import (
	"bytes"
	"fmt"
	"hash/fnv"
	"MagicInterpreter/ast"
	"strings"
)

// every value needs a different internal representation and it's easier to define two struct type
// than trying to fit booleans an integers into the same struct field
type ObjectType string

type Object interface {
	Type() ObjectType
	Inspect() string
}

// in order to fulfill the object.Object interface, we need Type() method that returns its ObjectType
// so we define constants for each objectType.
const (
	// integers
	INTEGER_OBJ = "INTEGER"

	// booleans
	BOOLEAN_OBJ = "BOOLEAN"

	// Null
	NULL_OBJ = "NULL"

	// Return Value
	RETURN_VALUE_OBJ = "RETURN_VALUE"

	// Error
	ERROR_OBJ = "ERROR"

	// Function Literal
	FUNCTION_OBJ = "FUNCTION"

	// String OBJ
	STRING_OBJ = "STRING"

	// Builtin OBJ
	BUILTIN_OBJ = "BUILTIN"

	// Array OBJ
	ARRAY_OBJ = "ARRAY"

	// HASH OBJ
	HASH_OBJ = "HASH"

	// FLOAT OBJ
	FLOAT_OBJ = "FLOAT"
)

// -------------------------
// Integer
// -------------------------
// when we encounter integer literal in the source ode we firstly turn it into a ast.IntegerLiteral(node)
// and when evaluating the AST node, we turn it into an object.Integer, saving the value inside our struct and passing
// around a reference to this struct

type Integer struct {
	Value int64
}

func (i *Integer) Inspect() string  { return fmt.Sprintf("%d", i.Value) }
func (i *Integer) Type() ObjectType { return INTEGER_OBJ }

// -------------------------
// Booleans
// -------------------------
// struct that wraps the single value, a bool i.e either false or true

type Boolean struct {
	Value bool
}

func (b *Boolean) Type() ObjectType { return BOOLEAN_OBJ }
func (b *Boolean) Inspect() string  { return fmt.Sprintf("%t", b.Value) }

// -------------------------
// Null Type
// -------------------------
// struct like object.boolean and object.integer which does not wrap any value
// it represents the absence of any kind of value
type Null struct{}

func (n *Null) Type() ObjectType { return NULL_OBJ }
func (n *Null) Inspect() string  { return "null" }

// -------------------------
// Return value
// -------------------------

// this will be just a wrapper to another object
type ReturnValue struct {
	Value Object
}

func (rv *ReturnValue) Type() ObjectType { return RETURN_VALUE_OBJ }
func (rv *ReturnValue) Inspect() string  { return rv.Value.Inspect() }

// -------------------------
// Error
// -------------------------

type Error struct {
	Message string
}

func (e *Error) Type() ObjectType { return ERROR_OBJ }
func (e *Error) Inspect() string  { return "ERROR: " + e.Message }

// ==============================
// Function Literal
// ==============================

type Function struct {
	Parameters []*ast.Identifier
	Body       *ast.BlockStatement
	Env        *Environment
}

func (f *Function) Type() ObjectType { return FUNCTION_OBJ }
func (f *Function) Inspect() string {
	var out bytes.Buffer

	params := []string{}
	for _, p := range f.Parameters {
		params = append(params, p.String())
	}

	out.WriteString("fn")
	out.WriteString("(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(") {\n")
	out.WriteString(f.Body.String())
	out.WriteString("\n}")

	return out.String()
}

// ==============================
// String Object
// ==============================

type String struct {
	Value string
}

func (s *String) Type() ObjectType { return STRING_OBJ }
func (s *String) Inspect() string  { return s.Value }

// ==============================
// builtin object
// ==============================
type BuiltinFunction func(args ...Object) Object

type Builtin struct {
	Fn BuiltinFunction
}

func (b *Builtin) Type() ObjectType { return BUILTIN_OBJ }
func (b *Builtin) Inspect() string  { return "builtin function" }

// ==============================
// ARRAY
// ==============================
type Array struct {
	Elements []Object
}

func (ao *Array) Type() ObjectType { return ARRAY_OBJ }
func (ao *Array) Inspect() string {
	var out bytes.Buffer
	elements := []string{}
	for _, e := range ao.Elements {
		elements = append(elements, e.Inspect())
	}
	out.WriteString("[")
	out.WriteString(strings.Join(elements, ", "))
	out.WriteString("]")
	return out.String()
}

// ==============================
// HashKey
// ==============================

type HashKey struct {
	Type  ObjectType
	Value uint64
}

func (b *Boolean) HashKey() HashKey {
	var value uint64

	if b.Value {
		value = 1
	} else {
		value = 0
	}

	return HashKey{Type: b.Type(), Value: value}
}

func (i *Integer) HashKey() HashKey {
	return HashKey{Type: i.Type(), Value: uint64(i.Value)}
}

func (s *String) HashKey() HashKey {
	h := fnv.New64a()
	h.Write([]byte(s.Value))

	return HashKey{Type: s.Type(), Value: h.Sum64()}
}

// ==============================
// Hash Struct
// ==============================

type HashPair struct {
	Key   Object
	Value Object
}

type Hash struct {
	Pairs map[HashKey]HashPair
}

func (h *Hash) Type() ObjectType { return HASH_OBJ }

// Inspect for *object.Hash : Hashing Inspect() method
func (h *Hash) Inspect() string {
	var out bytes.Buffer
	pairs := []string{}
	for _, pair := range h.Pairs {
		pairs = append(pairs, fmt.Sprintf("%s: %s",
			pair.Key.Inspect(), pair.Value.Inspect()))
	}
	out.WriteString("{")
	out.WriteString(strings.Join(pairs, ", "))
	out.WriteString("}")
	return out.String()
}

// interface for our evaluator to check if the given object is usable as hash key
// when we evaluate hash literals or index expression for hashes
type Hashable interface {
	HashKey() HashKey
}

// ========================
//	FloatLiteral
// ========================pe
type Float struct {
	Value float64
}

func (f *Float) Type() ObjectType { return FLOAT_OBJ }
func (f *Float) Inspect() string { return fmt.Sprintf("%g", f.Value)}
