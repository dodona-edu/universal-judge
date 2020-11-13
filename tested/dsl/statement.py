from ast import literal_eval
from os.path import join, dirname, realpath
from typing import Optional, Union, List

from lark import Lark, Tree, Token, UnexpectedToken

from tested.datatypes import string_to_type, AllTypes, SequenceTypes, \
    NumericTypes, BooleanTypes, NothingTypes, StringTypes, ObjectTypes, \
    BasicSequenceTypes, AdvancedSequenceTypes, BasicNumericTypes, \
    BasicStringTypes, \
    BasicBooleanTypes
from tested.serialisation import Statement, Assignment, NumberType, Expression, \
    NothingType, Identifier, BooleanType, StringType, SequenceType, FunctionCall, \
    ObjectType, FunctionType, VariableType, Value

data_types = (
    "INTEGER", "RATIONAL", "CHAR", "TEXT", "BOOLEAN", "SEQUENCE", "SET", "MAP",
    "NOTHING", "ANY", "INT8", "UINT8", "INT16", "UINT16", "INT32", "UINT32",
    "INT64", "UINT64", "BIGINT", "SINGLE_PRECISION", "DOUBLE_PRECISION",
    "DOUBLE_EXTENDED", "FIXED_PRECISION", "ARRAY", "LIST")

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
            grammar_file = join(dirname(realpath(__file__)), "grammar.lark")
        self.grammar_file = grammar_file
        self.parser = Lark.open(grammar_file, start=['stmt', 'return'],
                                lexer='standard')

    def analyse_arguments(self, tree: Tree) -> List[Expression]:
        if tree.data != 'args':
            raise ParseError("Function expect argument list")
        return [self.analyse_expression(arg, True) for arg in tree.children]

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
            var_type = expression.type
        else:
            # Type is given
            var_type = self.analyse_type_token(tree.children[index_type], True)
        # Create assignment object
        return Assignment(variable=var_name, expression=expression, type=var_type)

    def analyse_cast(self, tree: Tree, allow_functions: bool = True) -> Expression:
        expression = self.analyse_expression(tree.children[0], allow_functions)
        expr_type = string_to_type(tree.children[1].type)
        if isinstance(expression, SequenceType):
            if not isinstance(expr_type, SequenceTypes.__args__):
                raise ParseError("Can't cast sequence type to non-sequence type")
        elif isinstance(expression, NumberType):
            if not isinstance(expr_type, NumericTypes.__args__):
                raise ParseError("Can't cast numeric type to non-numeric type")
        elif isinstance(expression, BooleanType):
            if not isinstance(expr_type, BooleanTypes):
                raise ParseError("Can't cast numeric type to non-numeric type")
        elif isinstance(expression, NothingType):
            if not isinstance(expr_type, NothingTypes):
                raise ParseError("Can't cast nothing type to non-nothing type")
        elif isinstance(expression, StringType):
            if not isinstance(expr_type, StringTypes):
                raise ParseError("Can't cast string type to non-string type")
        elif isinstance(expression, ObjectType):
            if not isinstance(expr_type, ObjectTypes):
                raise ParseError("Can't cast object type to non-object type")
        else:
            raise ParseError("Non-value expression can not be casted")
        expression.type = expr_type
        return expression

    def analyse_constructor(self, tree: Tree) -> FunctionCall:
        # Find constructor name
        namespace, constr_name = self.analyse_namespace(tree.children[0])
        # Analyse arguments
        args = self.analyse_arguments(tree.children[1])
        return FunctionCall(type=FunctionType.CONSTRUCTOR,
                            name=constr_name,
                            namespace=namespace,
                            arguments=args)

    def analyse_dict(self, tree: Tree, allow_functions: bool = True) -> dict:
        def analyse_pair(pair: Tree) -> tuple:
            if pair.data != 'dict_pair':
                raise ParseError("Key-value pair expected in map")
            return (self.analyse_expression(pair.children[0]).data,
                    self.analyse_expression(pair.children[1], allow_functions))

        return dict(analyse_pair(pair) for pair in tree.children)

    def analyse_expression(self, tree: Union[Tree, Token],
                           allow_functions: bool = True) -> Expression:
        if isinstance(tree, Token):
            return self.analyse_expression_token(tree)
        return self.analyse_expression_tree(tree, allow_functions)

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
        raise ParseError("Invalid value token")

    def analyse_expression_tree(self, tree: Tree,
                                allow_functions: bool = True) -> Expression:
        if tree.data == 'null':
            return NothingType()
        elif tree.data in ('list', 'set', 'tuple'):
            return SequenceType(type=default_sequence_type_map[tree.data],
                                data=[
                                    self.analyse_expression(t, allow_functions)
                                    for t in tree.children
                                ])
        elif tree.data == 'value_cast':
            return self.analyse_cast(tree)
        elif tree.data == 'fun':
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
        raise ParseError("Invalid expression tree")

    def analyse_function(self, tree: Tree) -> FunctionCall:
        # Find function name
        namespace, fun_name = self.analyse_namespace(tree.children[0])
        # Analyse arguments
        args = self.analyse_arguments(tree.children[1])
        return FunctionCall(type=FunctionType.FUNCTION,
                            name=fun_name,
                            namespace=namespace,
                            arguments=args)

    def analyse_property(self, tree: Tree) -> FunctionCall:
        # Find namespace name
        namespace = self.analyse_name(tree.children[0])
        # Find property name
        prop_name = self.analyse_name(tree.children[1])
        return FunctionCall(type=FunctionType.PROPERTY,
                            name=prop_name,
                            namespace=namespace)

    def analyse_name(self, token: Token) -> str:
        if token.type != 'CNAME':
            raise ParseError("Invalid variable/function name")
        return token.value

    def analyse_namespace(self, tree: Tree):
        if tree.data != 'name':
            raise ParseError("Invalid name construction")
        if len(tree.children) == 1:
            return None, self.analyse_name(tree.children[0])
        return (self.analyse_name(tree.children[0]),
                self.analyse_name(tree.children[1]))

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
            parse_tree = self.parser.parse(statement, start='stmt')
            if isinstance(parse_tree, Tree) and parse_tree.data == 'assign':
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
