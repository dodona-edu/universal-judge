from ast import literal_eval
from os.path import join, dirname, realpath
from typing import Optional, Union, List, get_args, Tuple

from lark import Lark, Tree, Token, UnexpectedToken

from tested.datatypes import string_to_type, AllTypes, SequenceTypes, \
    NumericTypes, BooleanTypes, NothingTypes, StringTypes, ObjectTypes, \
    BasicSequenceTypes, AdvancedSequenceTypes, BasicNumericTypes, \
    BasicStringTypes, BasicBooleanTypes, AdvancedNothingTypes, BasicNothingTypes
from tested.serialisation import Statement, Assignment, NumberType, Expression, \
    NothingType, Identifier, BooleanType, StringType, SequenceType, FunctionCall, \
    ObjectType, FunctionType, VariableType, Value, ObjectKeyValuePair, Lambda, \
    TypedLambdaArgument, AllDataTypes, LambdaType

data_types = (
    "INTEGER", "RATIONAL", "CHAR", "TEXT", "BOOLEAN", "SEQUENCE", "SET", "MAP",
    "NOTHING", "ANY", "INT8", "UINT8", "INT16", "UINT16", "INT32", "UINT32",
    "INT64", "UINT64", "BIGINT", "SINGLE_PRECISION", "DOUBLE_PRECISION",
    "DOUBLE_EXTENDED", "FIXED_PRECISION", "ARRAY", "LIST"
)

default_sequence_type_map = {
    'list':  BasicSequenceTypes.SEQUENCE,
    'set':   BasicSequenceTypes.SET,
    'tuple': AdvancedSequenceTypes.TUPLE
}


class ParseError(Exception):
    pass


class Parser:
    def __init__(self, grammar_file: Optional[str] = None):
        if not grammar_file:
            grammar_file = join(dirname(realpath(__file__)), "tested.lark")
        self.grammar_file = grammar_file
        # Don't modify the parser, the parser must be 'lalr' for fast parsing
        # Modify the grammar instead to be unambiguous
        self.parser = Lark.open(grammar_file, start=['statements', 'return'],
                                lexer='standard', parser='lalr')

    def analyse_arguments(self, tree: Tree,
                          allow_functions: bool = True) -> List[Expression]:
        if tree.data != 'args':
            raise ParseError("Function expect argument list")
        return [self.analyse_expression(arg, allow_functions)
                for arg in tree.children]

    def analyse_lambda_parameter_types(self, tree: Tree) -> List[AllDataTypes]:
        return list(map(self.analyse_assign_datatype, tree.children))

    def analyse_lambda_return_type(self, tree: Tree) -> Optional[AllDataTypes]:
        if tree.children:
            return self.analyse_assign_datatype(tree.children[0])
        return None

    def analyse_assign_datatype(self, tree: Union[Tree, Token]) -> AllDataTypes:
        if isinstance(tree, Token):
            return self.analyse_type_token(tree, True)
        else:
            assert tree.data == "lambda_type"
            if tree.children:
                params = self.analyse_lambda_parameter_types(tree.children[0])
                return_type = self.analyse_lambda_return_type(tree.children[0])
                return LambdaType(
                    parameter_types=params,
                    return_type=return_type
                )
            else:
                return LambdaType()

    def analyse_assign(self, tree: Tree) -> Assignment:
        # Determine indices
        if len(tree.children) == 2:
            index_type, index_name, index_value = -1, 0, 1
        else:
            index_type, index_name, index_value = 0, 1, 2
        # Find variable name
        var_name = self.analyse_name(tree.children[index_name])
        # Parse expression
        expression = self.analyse_expression(tree.children[index_value], True)
        # Detect variable type
        if index_type == -1:
            # Type not given: derive
            if isinstance(expression, Value.__args__):
                var_type = expression.type
            elif isinstance(expression, FunctionCall):
                if expression.type == FunctionType.CONSTRUCTOR:
                    var_type = VariableType(data=expression.name)
                else:
                    raise ParseError(
                        "Can't derive variable type from function call")
            elif isinstance(expression, Lambda):
                raise ParseError("Can't derive variable type from lambda")
            else:
                raise ParseError("Can't derive variable type from identifier")
        else:
            # Type is given
            var_type = self.analyse_assign_datatype(tree.children[index_type])
        # Create assignment object
        return Assignment(variable=var_name, expression=expression, type=var_type)

    def analyse_cast(self, tree: Tree, allow_functions: bool = True) -> Expression:
        expression = self.analyse_expression(tree.children[0], allow_functions)
        expr_type = string_to_type(tree.children[1].type)
        if isinstance(expression, SequenceType):
            if not isinstance(expr_type, get_args(SequenceTypes)):
                raise ParseError("Can't cast sequence type to non-sequence type")
        elif isinstance(expression, NumberType):
            if not isinstance(expr_type, get_args(NumericTypes)):
                raise ParseError("Can't cast numeric type to non-numeric type")
        elif isinstance(expression, BooleanType):
            if not isinstance(expr_type, BooleanTypes):
                raise ParseError("Can't cast numeric type to non-numeric type")
        elif isinstance(expression, NothingType):
            if not isinstance(expr_type, NothingTypes):
                raise ParseError("Can't cast nothing type to non-nothing type")
        elif isinstance(expression, StringType):
            if not isinstance(expr_type, get_args(StringTypes)):
                raise ParseError("Can't cast string type to non-string type")
        elif isinstance(expression, ObjectType):
            if isinstance(expr_type, get_args(SequenceTypes)):
                if len(expression.data) == 0:
                    return SequenceType(type=expr_type, data=[])
                else:
                    raise ParseError("Can't cast non-empty dict to non-object type")
            elif not isinstance(expr_type, ObjectTypes):
                raise ParseError("Can't cast object type to non-object type")
        else:
            raise ParseError("Non-value expression can not be casted")
        expression.type = expr_type
        return expression

    def analyse_constructor(self, tree: Tree) -> FunctionCall:
        # Analyse constructor function
        function = self.analyse_function(tree.children[0])
        # Update function type
        function.type = FunctionType.CONSTRUCTOR
        return function

    def analyse_constructor_reference(self, tree: Tree) -> FunctionCall:
        # Analyse constructor function
        function = self.analyse_function_reference(tree)
        # Update function type
        function.type = FunctionType.CONSTRUCTOR_REFERENCE
        return function

    def analyse_dict(self, tree: Tree,
                     allow_functions: bool = True) -> List[ObjectKeyValuePair]:
        def analyse_pair(pair: Tree) -> ObjectKeyValuePair:
            if pair.data != 'dict_pair':
                raise ParseError("Key-value pair expected in map")
            return ObjectKeyValuePair(key=self.analyse_expression(pair.children[0],
                                                                  allow_functions),
                                      value=self.analyse_expression(
                                          pair.children[1], allow_functions))

        return [analyse_pair(pair) for pair in tree.children]

    def analyse_expression(self, tree: Union[Tree, Token],
                           allow_functions: bool = True) -> Expression:
        if isinstance(tree, Token):
            return self.analyse_expression_token(tree)
        return self.analyse_expression_tree(tree, allow_functions)

    # noinspection PyMethodMayBeStatic
    def analyse_expression_token(self, token: Token) -> Expression:
        if token.type == 'CNAME':
            return Identifier.validate(token.value)
        elif token.type == 'SIGNED_INT':
            return NumberType(type=BasicNumericTypes.INTEGER, data=int(token.value))
        elif token.type == 'SIGNED_FLOAT':
            return NumberType(type=BasicNumericTypes.RATIONAL,
                              data=float(token.value))
        elif token.type == 'ESCAPED_STRING':
            return StringType(type=BasicStringTypes.TEXT,
                              data=literal_eval(token.value))
        elif token.type == 'TRUE':
            return BooleanType(type=BasicBooleanTypes.BOOLEAN, data=True)
        elif token.type == 'FALSE':
            return BooleanType(type=BasicBooleanTypes.BOOLEAN, data=False)
        elif token.type == 'NULL':
            return NothingType(type=BasicNothingTypes.NOTHING)
        elif token.type == 'UNDEFINED':
            return NothingType(type=AdvancedNothingTypes.UNDEFINED)
        raise ParseError("Invalid value token")

    def analyse_expression_tree(self, tree: Tree,
                                allow_functions: bool = True) -> Expression:
        if tree.data in ('list', 'set', 'tuple'):
            if tree.data == 'set':
                tree.children = [Tree(data="args", children=tree.children)]
            content = self.analyse_arguments(tree.children[0],
                                             allow_functions=allow_functions)
            return SequenceType(type=default_sequence_type_map[tree.data],
                                data=content)
        elif tree.data == 'value_cast':
            return self.analyse_cast(tree)
        elif tree.data == 'function':
            if allow_functions:
                return self.analyse_function(tree)
            else:
                raise ParseError("Function call not allowed for return values")
        elif tree.data == 'constructor':
            if allow_functions:
                return self.analyse_constructor(tree)
            else:
                raise ParseError("Constructor not allowed for return values")
        elif tree.data == 'property':
            if allow_functions:
                return self.analyse_property(tree)
            else:
                raise ParseError("Property not allowed for return values")
        elif tree.data == 'dict':
            return ObjectType(type=ObjectTypes.MAP,
                              data=self.analyse_dict(tree, allow_functions))
        elif tree.data == 'global_variable':
            if allow_functions:
                return self.analyse_global_variable(tree)
            else:
                raise ParseError("Global variables not allowed for return values")
        elif tree.data == "function_reference":
            if allow_functions:
                return self.analyse_function_reference(tree)
            else:
                raise ParseError(
                    "Function references not allowed for return values")
        elif tree.data == "constructor_reference":
            if allow_functions:
                return self.analyse_constructor_reference(tree)
            else:
                raise ParseError(
                    "Constructor references not allowed for return values")
        elif tree.data == "lambda":
            return self.analyse_lambda(tree)
        raise ParseError("Invalid expression tree")

    def analyse_function(self, tree: Tree) -> FunctionCall:
        # Find function name
        namespace, fun_name = self.analyse_namespace(tree.children[0])
        # Analyse arguments
        args = self.analyse_arguments(tree.children[1], allow_functions=True)
        return FunctionCall(type=FunctionType.FUNCTION,
                            name=fun_name,
                            namespace=namespace,
                            arguments=args)

    def analyse_function_reference(self, tree: Tree) -> FunctionCall:
        if len(tree.children) == (1 if tree.data == "function_reference" else 2):
            namespace, name = None, self.analyse_name(tree.children[0])
        else:
            namespace = self.analyse_expression(tree.children[0],
                                                allow_functions=True)
            name = self.analyse_name(tree.children[1])

        return FunctionCall(type=FunctionType.FUNCTION_REFERENCE,
                            name=name,
                            namespace=namespace)

    # noinspection PyMethodMayBeStatic
    def analyse_global_variable(self, tree: Tree) -> FunctionCall:
        return FunctionCall(type=FunctionType.PROPERTY,
                            name=self.analyse_name(tree.children[0]))

    def analyse_typed_lambda_parameter(self, tree: Tree) -> TypedLambdaArgument:
        type_ = self.analyse_type_token(tree.children[0], assign=True)
        name = self.analyse_name(tree.children[1])
        return TypedLambdaArgument(
            type=type_,
            name=name
        )

    def analyse_lambda_parameters(self, tree: Tree
                                  ) -> Union[List[str], List[TypedLambdaArgument]]:
        if not tree.children:
            return []
        if tree.data == 'typed_lambda_parameters':
            return list(map(self.analyse_typed_lambda_parameter, tree.children))
        return list(map(self.analyse_name, tree.children))

    def analyse_lambda(self, tree: Tree) -> Lambda:
        parameters = self.analyse_lambda_parameters(tree.children[0])
        body = self.analyse_expression(tree.children[1], True)
        return Lambda(
            body=body,
            parameters=parameters
        )

    def analyse_property(self, tree: Tree) -> FunctionCall:
        # Find namespace name
        namespace = self.analyse_expression(tree.children[0])
        # Find property name
        prop_name = self.analyse_name(tree.children[1])
        return FunctionCall(name=prop_name,
                            namespace=namespace,
                            type=FunctionType.PROPERTY)

    # noinspection PyMethodMayBeStatic
    def analyse_name(self, token: Token) -> str:
        if token.type != 'CNAME':
            raise ParseError("Invalid variable/function name")
        return token.value

    def analyse_namespace(self, tree: Union[Tree, Token]
                          ) -> Tuple[Optional[Expression], str]:
        expression = self.analyse_expression(tree, allow_functions=True)
        if isinstance(expression, str):
            return None, expression
        return expression.namespace, expression.name

    # noinspection PyMethodMayBeStatic
    def analyse_type_token(self, token: Token,
                           assign=False) -> Union[VariableType, AllTypes]:
        if assign and token.type == 'CNAME':
            return VariableType(data=token.value)
        elif token.type in data_types:
            return string_to_type(token.type)
        else:
            raise ParseError("Invalid variable type")

    def parse_statement(self, statement: str) -> Statement:
        try:
            parse_tree = self.parser.parse(statement, start='statements')
            if isinstance(parse_tree, Tree) and parse_tree.data == 'assignment':
                return self.analyse_assign(parse_tree)
            return self.analyse_expression(parse_tree, True)
        except UnexpectedToken as e:
            raise ParseError("Bad token") from e

    def parse_value(self, value: str) -> Value:
        try:
            parse_tree = self.parser.parse(value, start='return')
            return self.analyse_expression(parse_tree, False)
        except UnexpectedToken as e:
            raise ParseError("Bad token") from e
