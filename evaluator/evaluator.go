package evaluator

import (
	"MagicInterpreter/ast"
	"MagicInterpreter/object"
	"fmt"
)

// instead of creating new instance every time, we reference them instead
var (
	NULL  = &object.Null{}
	TRUE  = &object.Boolean{Value: true}
	FALSE = &object.Boolean{Value: false}
)

func Eval(node ast.Node, env *object.Environment) object.Object {
	switch node := node.(type) {

	// Statements
	case *ast.Program:
		return evalProgram(node, env)

	case *ast.ExpressionStatement:
		return Eval(node.Expression, env)

	// Boolean
	case *ast.Boolean:
		return nativeBoolToBooleanObject(node.Value)

	// Expression
	case *ast.IntegerLiteral:
		return &object.Integer{Value: node.Value}

	// prefixExpression
	case *ast.PrefixExpression:
		right := Eval(node.Right, env)
		if isError(right) {
			return right
		}
		return evalPrefixExpression(node.Operator, right)

	// infixExpression
	case *ast.InfixExpression:
		left := Eval(node.Left, env)
		right := Eval(node.Right, env)

		if isError(left) {
			return left
		}

		if isError(right) {
			return right
		}

		return evalInfixExpression(node.Operator, left, right)

	// BlockStatement
	case *ast.BlockStatement:
		return evalBlockStatement(node, env)

	// IfExpression
	case *ast.IfExpression:
		return evalIfExpression(node, env)

	// ReturnStatement
	case *ast.ReturnStatement:
		if node.ReturnValue == nil {
			return &object.ReturnValue{Value: NULL}
		}
		val := Eval(node.ReturnValue, env)
		if isError(val) {
			return val
		}
		return &object.ReturnValue{Value: val}

	// LetStatement
	case *ast.LetStatement:
		if node.Value == nil {
			return newError("let statement has no value")
		}
		val := Eval(node.Value, env)
		if isError(val) {
			return val
		}
		env.Set(node.Name.Value, val)
		return NULL

	// Identifier
	case *ast.Identifier:
		return evalIdentifier(node, env)

	// FunctionLiteral
	case *ast.FunctionLiteral:
		params := node.Parameters
		body := node.Body
		return &object.Function{Parameters: params, Env: env, Body: body}

	// CallExpression
	case *ast.CallExpression:
		function := Eval(node.Function, env)
		if isError(function) {
			return function
		}

		args := evalExpressions(node.Arguments, env)
		if len(args) == 1 && isError(args[0]) {
			return args[0]
		}

		return applyFunction(function, args)

	// IndexExpression
	case *ast.IndexExpression:
		left := Eval(node.Left, env)
		if isError(left) {
			return left
		}
		index := Eval(node.Index, env)

		if isError(index) {
			return index
		}

		return evalIndexExpression(left, index)

	// StringLiteral
	case *ast.StringLiteral:
		return &object.String{Value: node.Value}

	// ArrayLiteral
	// evaluates *ast.ArrayLiterals and produces array objects
	case *ast.ArrayLiteral:
		elements := evalExpressions(node.Elements, env)
		if len(elements) == 1 && isError(elements[0]) {
			return elements[0]
		}
		return &object.Array{Elements: elements}

	// HashLiteral
	case *ast.HashLiteral:
		return evalHashLiteral(node, env)

	// FloatLiteral
	case *ast.FloatLiteral:
		return &object.Float{Value: node.Value}
	}
	return newError("unknown node type: %T", node)
}

// evaluate Statements
// we check if the last evaluation result is object.ReturnValue and
// if so we stop the evaluation and return the unwrapped value
// we don't return object.ReturnValue, but only the value it's wrapping
func evalStatements(stmts []ast.Statement, env *object.Environment) object.Object {
	var result object.Object

	for _, statement := range stmts {
		result = Eval(statement, env)

		// return value
		if returnValue, ok := result.(*object.ReturnValue); ok {
			return returnValue.Value
		}
	}
	return result
}

// since each time we encounter boolean instead of creating new instances we create reference
// that reference to TRUE and FALSE

func nativeBoolToBooleanObject(input bool) *object.Boolean {
	if input {
		return TRUE
	}
	return FALSE
}

// take right operand and pass it to evalPrefixExpression which checks if the operator is supported
func evalPrefixExpression(Operator string, right object.Object) object.Object {
	switch Operator {
	case "!":
		return evalBangOperatorExpression(right)
	case "-":
		return evalMinusPrefixOperatorExpression(right)
	default:
		return newError("unknown operator: %s%s", Operator, right.Type())
	}
}

// specifying the behavior of the `!` operator
func evalBangOperatorExpression(right object.Object) object.Object {
	switch right {
	case TRUE:
		return FALSE
	case FALSE:
		return TRUE
	case NULL:
		return TRUE
	default:
		return FALSE
	}
}

// check if the operand is integer, If it is not we return NULL
// if it is we extract the value of the *object.Integer
// then allocate a new object to wrap a negated version of this value
func evalMinusPrefixOperatorExpression(right object.Object) object.Object {
	if right.Type() != object.INTEGER_OBJ {
		return newError("unknown operator: -%s", right.Type())
	}

	value := right.(*object.Integer).Value
	return &object.Integer{Value: -value}
}

// we are checking the infixExpression and if both are not integer(left and right) we return NULL
// if it is integer they are wrapped by *object.Integers and
// are added, subtracted, division, multiplied
func evalInfixExpression(
	operator string,
	left, right object.Object,
) object.Object {
	switch {
	// both  ints
	case left.Type() == object.INTEGER_OBJ && right.Type() == object.INTEGER_OBJ:
		return evalIntegerInfixExpression(operator, left, right)

	// FLoat Number : both number are floats
	case left.Type() == object.FLOAT_OBJ && right.Type() == object.FLOAT_OBJ:
		return evalFloatInfixExpression(operator, left, right)

	// left side is float and other side is int
	case left.Type() == object.FLOAT_OBJ && right.Type() == object.INTEGER_OBJ:
		rightFloat := &object.Float{Value: float64(right.(*object.Integer).Value)}
		return evalFloatInfixExpression(operator, left, rightFloat)

	// left is int and right side is float
	case left.Type() == object.INTEGER_OBJ && right.Type() == object.FLOAT_OBJ:
		leftFloat := &object.Float{Value: float64(left.(*object.Integer).Value)}
		return evalFloatInfixExpression(operator, leftFloat, right)
	case operator == "==":
		return nativeBoolToBooleanObject(left == right)
	case operator == "!=":
		return nativeBoolToBooleanObject(left != right)
	case left.Type() != right.Type():
		return newError("type mismatch: %s %s %s",
			left.Type(), operator, right.Type())
	// string concatenation
	case left.Type() == object.STRING_OBJ && right.Type() == object.STRING_OBJ:
		return evalStringInfixExpression(operator, left, right)
	default:
		return newError("unknown operator: %s %s %s",
			left.Type(), operator, right.Type())
	}
}

// evaluation of integerInfix Expression
func evalIntegerInfixExpression(
	operator string,
	left, right object.Object,
) object.Object {
	leftVal := left.(*object.Integer).Value
	rightVal := right.(*object.Integer).Value

	switch operator {
	case "+":
		return &object.Integer{Value: leftVal + rightVal}
	case "-":
		return &object.Integer{Value: leftVal - rightVal}
	case "*":
		return &object.Integer{Value: leftVal * rightVal}
	case "/":
		return &object.Integer{Value: leftVal / rightVal}
	case "<":
		return nativeBoolToBooleanObject(leftVal < rightVal)
	case ">":
		return nativeBoolToBooleanObject(leftVal > rightVal)
	case "==":
		return nativeBoolToBooleanObject(leftVal == rightVal)
	case "!=":
		return nativeBoolToBooleanObject(leftVal != rightVal)
	default:
		return newError("unknown opeartor: %s %s %s", left.Type(), operator, right.Type())
	}
}

// evaluate the IfExpression
func evalIfExpression(ie *ast.IfExpression, env *object.Environment) object.Object {
	condition := Eval(ie.Condition, env)
	if isError(condition) {
		return condition
	}

	if istruthy(condition) {
		return Eval(ie.Consequence, env)
	} else if ie.Alternative != nil {
		return Eval(ie.Alternative, env)
	} else {
		return NULL
	}
}

// the istruthy function to check if its true or false
func istruthy(obj object.Object) bool {
	switch obj {
	case NULL:
		return false
	case TRUE:
		return true
	case FALSE:
		return false
	default:
		return true
	}
}

// evalProgram evaluation of nodes
func evalProgram(program *ast.Program, env *object.Environment) object.Object {
	var result object.Object

	for _, statement := range program.Statements {
		result = Eval(statement, env)

		switch result := result.(type) {
		case *object.ReturnValue:
			return result.Value
		case *object.Error:
			return result
		}
	}
	return result
}

// evaluation of block statements
func evalBlockStatement(block *ast.BlockStatement, env *object.Environment) object.Object {
	var result object.Object

	for _, statement := range block.Statements {
		result = Eval(statement, env)

		if result != nil {
			rt := result.Type()
			if rt == object.RETURN_VALUE_OBJ || rt == object.ERROR_OBJ {
				return result
			}
		}
	}
	return result
}

// Error
func newError(format string, a ...interface{}) *object.Error {
	return &object.Error{Message: fmt.Sprintf(format, a...)}
}

// isError
func isError(obj object.Object) bool {
	if obj != nil {
		return obj.Type() == object.ERROR_OBJ
	}
	return false
}

// evaluating Identifier
func evalIdentifier(
	node *ast.Identifier,
	env *object.Environment,
) object.Object {
	if val, ok := env.Get(node.Value); ok {
		return val
	}

	// lookup builtins in function
	if builtin, ok := builtins[node.Value]; ok {
		return builtin
	}
	return newError("%s", "identifier not found: "+node.Value)
}

// evaluating the Expressions
// iterate through the list of ast.expression/Nodes and evaluate them in the context of the current environment
// stop if encounter error and return error
// evaluate the arguments from left to right
func evalExpressions(
	exps []ast.Expression,
	env *object.Environment,
) []object.Object {
	var result []object.Object

	for _, e := range exps {
		evaluated := Eval(e, env)
		if isError(evaluated) {
			return []object.Object{evaluated}
		}
		result = append(result, evaluated)
	}
	return result
}

// we check if we really have a *object.Function at hand but also convert the fn parameter
//
//	to a *object.Function reference in order to get access to the function's env and body fields
func applyFunction(fn object.Object, args []object.Object) object.Object {
	switch fn := fn.(type) {
	case *object.Function:
		extendEnv := extendFunctionEnv(fn, args)
		evaluated := Eval(fn.Body, extendEnv)
		return unwrapReturnValue(evaluated)

	case *object.Builtin:
		return fn.Fn(args...)

	default:
		return newError("not a function: %s", fn.Type())
	}
}

// creates a new *object.Environment that's enclosed by the function's environment

func extendFunctionEnv(
	fn *object.Function,
	args []object.Object,
) *object.Environment {
	env := object.NewEnclosedEnvironment(fn.Env)

	for paramIdx, param := range fn.Parameters {
		env.Set(param.Value, args[paramIdx])
	}

	return env
}

func unwrapReturnValue(obj object.Object) object.Object {
	if returnValue, ok := obj.(*object.ReturnValue); ok {
		return returnValue.Value
	}

	return obj
}

// evaluate stringInfixExpression
func evalStringInfixExpression(
	operator string,
	left, right object.Object,
) object.Object {
	if operator != "+" {
		return newError("unknown operator: %s %s %s", left.Type(), operator, right.Type())
	}

	leftVal := left.(*object.String).Value
	rightVal := right.(*object.String).Value

	return &object.String{Value: leftVal + rightVal}
}

// evaluating Index Expression
func evalIndexExpression(left, index object.Object) object.Object {
	switch {
	case left.Type() == object.ARRAY_OBJ && index.Type() == object.INTEGER_OBJ:
		return evalArrayIndexExpression(left, index)
	case left.Type() == object.HASH_OBJ:
		return evalHashIndexExpression(left, index)
	default:
		return newError("index operator not supported: %s", left.Type())
	}
}

// evalArrayIndexExpression
// retrieve the element with the specified index from the array
// checks if the given index is out of range and if that's the case it returns NULL
// otherwise desired element
func evalArrayIndexExpression(array, index object.Object) object.Object {
	arrayObject := array.(*object.Array)
	idx := index.(*object.Integer).Value
	max := int64(len(arrayObject.Elements) - 1)

	if idx < 0 || idx > max {
		return NULL
	}
	return arrayObject.Elements[idx]
}

// evaluating the HashLiterals
func evalHashLiteral(
	node *ast.HashLiteral,
	env *object.Environment,
) object.Object {
	pairs := make(map[object.HashKey]object.HashPair)

	for keyNode, valueNode := range node.Pairs {
		key := Eval(keyNode, env)
		if isError(key) {
			return key
		}

		hashKey, ok := key.(object.Hashable)
		if !ok {
			return newError("unusable as hash key: %s", key.Type())
		}

		value := Eval(valueNode, env)
		if isError(value) {
			return value
		}

		hashed := hashKey.HashKey()
		pairs[hashed] = object.HashPair{Key: key, Value: value}
	}

	return &object.Hash{Pairs: pairs}
}

// evalHashIndexExpression
func evalHashIndexExpression(hash, index object.Object) object.Object {
	hashObject := hash.(*object.Hash)

	key, ok := index.(object.Hashable)
	if !ok {
		return newError("unusable as hash key: %s", index.Type())
	}
	pair, ok := hashObject.Pairs[key.HashKey()]
	if !ok {
		return NULL
	}
	return pair.Value
}

// FLoat evaluation Infix Expression

func evalFloatInfixExpression(operator string, left, right object.Object) object.Object {
	leftVal := left.(*object.Float).Value
	rightVal := right.(*object.Float).Value

	switch operator {

	case "+":
		return &object.Float{Value: leftVal + rightVal}
	case "-":
		return &object.Float{Value: leftVal - rightVal}
	case "*":
		return &object.Float{Value: leftVal * rightVal}
	case "/":
		return &object.Float{Value: leftVal / rightVal}
	case "<":
		return nativeBoolToBooleanObject(leftVal < rightVal)
	case ">":
		return nativeBoolToBooleanObject(leftVal > rightVal)
	case "==":
		return nativeBoolToBooleanObject(leftVal == rightVal)
	case "!=":
		return nativeBoolToBooleanObject(leftVal != rightVal)
	default:
		return newError("unknown operator: %s %s %s", left.Type(), operator, right.Type())
	}
}
