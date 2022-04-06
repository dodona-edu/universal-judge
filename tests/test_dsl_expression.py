import math

import pytest

from tested.datatypes import (
    BasicNothingTypes,
    BasicNumericTypes,
    AdvancedNumericTypes,
    BasicBooleanTypes,
    BasicStringTypes,
    BasicSequenceTypes,
    AdvancedSequenceTypes,
    ObjectTypes,
    AdvancedStringTypes,
    AdvancedNothingTypes,
)
from tested.dsl import Parser, ParseError
from tested.serialisation import (
    Assignment,
    FunctionCall,
    FunctionType,
    VariableType,
    SequenceType,
    Identifier,
    StringType,
    ObjectKeyValuePair,
    ObjectType,
)

parser = Parser()


def test_parse_value_null():
    parsed = parser.parse_value("null")
    assert parsed.type == BasicNothingTypes.NOTHING
    assert parsed.data is None


def test_parse_value_undefined():
    parsed = parser.parse_value("undefined")
    assert parsed.type == AdvancedNothingTypes.UNDEFINED
    assert parsed.data is None


def test_parse_value_pos_integer():
    parsed = parser.parse_value("5")
    assert parsed.type == BasicNumericTypes.INTEGER
    assert parsed.data == 5


def test_parse_value_neg_integer():
    parsed = parser.parse_value("-10")
    assert parsed.type == BasicNumericTypes.INTEGER
    assert parsed.data == -10


def test_parse_value_integer():
    parsed = parser.parse_value("-10 :: integer")
    assert parsed.type == BasicNumericTypes.INTEGER
    assert parsed.data == -10


def test_parse_value_bigint():
    parsed = parser.parse_value("-1024 :: bigint")
    assert parsed.type == AdvancedNumericTypes.BIG_INT
    assert parsed.data == -1024


def test_parse_value_uint8():
    parsed = parser.parse_value("5 :: uint8")
    assert parsed.type == AdvancedNumericTypes.U_INT_8
    assert parsed.data == 5


def test_parse_value_int8():
    parsed = parser.parse_value("-10 :: int8")
    assert parsed.type == AdvancedNumericTypes.INT_8
    assert parsed.data == -10


def test_parse_value_uint16():
    parsed = parser.parse_value("5 :: uint16")
    assert parsed.type == AdvancedNumericTypes.U_INT_16
    assert parsed.data == 5


def test_parse_value_int16():
    parsed = parser.parse_value("-10 :: int16")
    assert parsed.type == AdvancedNumericTypes.INT_16
    assert parsed.data == -10


def test_parse_value_uint32():
    parsed = parser.parse_value("5 :: uint32")
    assert parsed.type == AdvancedNumericTypes.U_INT_32
    assert parsed.data == 5


def test_parse_value_int32():
    parsed = parser.parse_value("-10 :: int32")
    assert parsed.type == AdvancedNumericTypes.INT_32
    assert parsed.data == -10


def test_parse_value_uint64():
    parsed = parser.parse_value("5 :: uint64")
    assert parsed.type == AdvancedNumericTypes.U_INT_64
    assert parsed.data == 5


def test_parse_value_int64():
    parsed = parser.parse_value("-10 :: int64")
    assert parsed.type == AdvancedNumericTypes.INT_64
    assert parsed.data == -10


def test_parse_value_real():
    parsed = parser.parse_value("1.0")
    assert parsed.type == BasicNumericTypes.REAL
    assert math.isclose(parsed.data, 1.0)


def test_parse_value_real_neg():
    parsed = parser.parse_value("-1.0 :: real")
    assert parsed.type == BasicNumericTypes.REAL
    assert math.isclose(parsed.data, -1.0)


def test_parse_value_single():
    parsed = parser.parse_value("1e-10 :: single")
    assert parsed.type == AdvancedNumericTypes.SINGLE_PRECISION
    assert math.isclose(parsed.data, 1e-10)


def test_parse_value_double():
    parsed = parser.parse_value("+2.5e+10 :: double")
    assert parsed.type == AdvancedNumericTypes.DOUBLE_PRECISION
    assert math.isclose(parsed.data, 2.5e10)


def test_parse_value_double_extended():
    parsed = parser.parse_value("-5.5e5 :: extended")
    assert parsed.type == AdvancedNumericTypes.DOUBLE_EXTENDED
    assert math.isclose(parsed.data, -5.5e5)


def test_parse_value_fixed_precision():
    parsed = parser.parse_value("+5.5 :: fixed")
    assert parsed.type == AdvancedNumericTypes.FIXED_PRECISION
    assert math.isclose(parsed.data, 5.5)


def test_parse_value_true():
    parsed = parser.parse_value("true")
    assert parsed.type == BasicBooleanTypes.BOOLEAN
    assert parsed.data is True


def test_parse_value_false():
    parsed = parser.parse_value("false :: boolean")
    assert parsed.type == BasicBooleanTypes.BOOLEAN
    assert parsed.data is False


def test_parse_value_text():
    parsed = parser.parse_value('"this is a string"')
    assert parsed.type == BasicStringTypes.TEXT
    assert parsed.data == "this is a string"


def test_parse_value_text_cast():
    parsed = parser.parse_value(r'"this\nis\na\nstring" :: text')
    assert parsed.type == BasicStringTypes.TEXT
    assert parsed.data == "this\nis\na\nstring"


def test_parse_value_char():
    parsed = parser.parse_value('"c" :: char')
    assert parsed.type == AdvancedStringTypes.CHAR
    assert parsed.data == "c"


def test_parse_value_sequence():
    parsed = parser.parse_value('[5, "text"]')
    assert parsed.type == BasicSequenceTypes.SEQUENCE
    assert len(parsed.data) == 2
    assert parsed.data[0].type == BasicNumericTypes.INTEGER
    assert parsed.data[0].data == 5
    assert parsed.data[1].type == BasicStringTypes.TEXT
    assert parsed.data[1].data == "text"


def test_parse_value_set():
    parsed = parser.parse_value('{"d" :: char, 8 :: uint8}')
    assert parsed.type == BasicSequenceTypes.SET
    assert len(parsed.data) == 2
    assert parsed.data[0].type == AdvancedStringTypes.CHAR
    assert parsed.data[0].data == "d"
    assert parsed.data[1].type == AdvancedNumericTypes.U_INT_8
    assert parsed.data[1].data == 8


def test_parse_value_tuple():
    parsed = parser.parse_value("(true, false)")
    assert parsed.type == AdvancedSequenceTypes.TUPLE
    assert len(parsed.data) == 2
    assert parsed.data[0].type == BasicBooleanTypes.BOOLEAN
    assert parsed.data[0].data is True
    assert parsed.data[1].type == BasicBooleanTypes.BOOLEAN
    assert parsed.data[1].data is False


def test_parse_value_adv_sequence():
    parsed = parser.parse_value('[5, ["text", "data"] :: array] :: list')
    assert parsed.type == AdvancedSequenceTypes.LIST
    assert len(parsed.data) == 2
    assert parsed.data[0].type == BasicNumericTypes.INTEGER
    assert parsed.data[0].data == 5
    assert parsed.data[1].type == AdvancedSequenceTypes.ARRAY
    assert len(parsed.data[1].data) == 2
    assert parsed.data[1].data[0].type == BasicStringTypes.TEXT
    assert parsed.data[1].data[0].data == "text"
    assert parsed.data[1].data[1].type == BasicStringTypes.TEXT
    assert parsed.data[1].data[1].data == "data"


def test_parse_value_dict():
    parsed = parser.parse_value('{"ignore": true, 5: 0}')
    assert parsed.type == ObjectTypes.MAP
    parsed: ObjectType
    assert len(parsed.data) == 2
    key, value = parsed.data[0].key, parsed.data[0].value
    assert key.type == BasicStringTypes.TEXT
    assert key.data == "ignore"
    assert value.type == BasicBooleanTypes.BOOLEAN
    assert value.data is True
    key, value = parsed.data[1].key, parsed.data[1].value
    assert key.type == BasicNumericTypes.INTEGER
    assert key.data == 5
    assert value.type == BasicNumericTypes.INTEGER
    assert value.data == 0


def test_parse_error_fun_in_return_value():
    with pytest.raises(ParseError):
        parser.parse_value("[fun()]")


def test_parse_error_property_in_return_value():
    with pytest.raises(ParseError):
        parser.parse_value("{data.data}")


def test_parse_error_constructor_in_return_value():
    with pytest.raises(ParseError):
        parser.parse_value('{"data": new data.Object()}')


def test_parse_error_constructor_in_return_value2():
    with pytest.raises(ParseError):
        parser.parse_value('{new data.Object(): "data"}')


def test_parse_error_fun_assign():
    with pytest.raises(ParseError):
        parser.parse_statement("data = first([object.gen_int()])")


def test_parse_fun_assign():
    assign = parser.parse_statement("integer data = first([object.gen_int()])")
    assert isinstance(assign, Assignment)
    assert assign.type == BasicNumericTypes.INTEGER
    assert assign.variable == "data"
    expr = assign.expression
    assert isinstance(expr, FunctionCall)
    assert expr.namespace is None
    assert expr.name == "first"
    assert expr.type == FunctionType.FUNCTION
    assert len(expr.arguments) == 1
    arg = expr.arguments[0]
    assert arg.type == BasicSequenceTypes.SEQUENCE
    assert len(arg.data) == 1
    data = arg.data[0]
    assert isinstance(data, FunctionCall)
    assert data.type == FunctionType.FUNCTION
    assert data.namespace == "object"
    assert data.name == "gen_int"
    assert len(data.arguments) == 0


def test_parse_constructor_assign():
    assign = parser.parse_statement("Container cont = new Container({object.version})")
    assert isinstance(assign, Assignment)
    assert isinstance(assign.type, VariableType)
    assert assign.type.data == "Container"
    assert assign.variable == "cont"
    expr = assign.expression
    assert isinstance(expr, FunctionCall)
    assert expr.namespace is None
    assert expr.name == "Container"
    assert expr.type == FunctionType.CONSTRUCTOR
    assert len(expr.arguments) == 1
    arg = expr.arguments[0]
    assert arg.type == BasicSequenceTypes.SET
    assert len(arg.data) == 1
    data = arg.data[0]
    assert isinstance(data, FunctionCall)
    assert data.type == FunctionType.PROPERTY
    assert data.namespace == "object"
    assert data.name == "version"
    assert len(data.arguments) == 0


def test_parse_constructor_assign2():
    assign = parser.parse_statement("cont = new Container({object.version})")
    assert isinstance(assign, Assignment)
    assert isinstance(assign.type, VariableType)
    assert assign.type.data == "Container"
    assert assign.variable == "cont"
    expr = assign.expression
    assert isinstance(expr, FunctionCall)
    assert expr.namespace is None
    assert expr.name == "Container"
    assert expr.type == FunctionType.CONSTRUCTOR
    assert len(expr.arguments) == 1
    arg = expr.arguments[0]
    assert arg.type == BasicSequenceTypes.SET
    assert len(arg.data) == 1
    data = arg.data[0]
    assert isinstance(data, FunctionCall)
    assert data.type == FunctionType.PROPERTY
    assert data.namespace == "object"
    assert data.name == "version"
    assert len(data.arguments) == 0


def test_parse_value_assign():
    assign = parser.parse_statement("list lijst = [new Container(5, True)] :: list")
    assert isinstance(assign, Assignment)
    assert assign.type == AdvancedSequenceTypes.LIST
    assert assign.variable == "lijst"
    expr = assign.expression
    assert isinstance(expr, SequenceType)
    assert expr.type == AdvancedSequenceTypes.LIST
    assert len(expr.data) == 1
    elem = expr.data[0]
    assert isinstance(elem, FunctionCall)
    assert elem.type == FunctionType.CONSTRUCTOR
    assert elem.namespace is None
    assert elem.name == "Container"
    assert len(elem.arguments) == 2
    assert elem.arguments[0].type == BasicNumericTypes.INTEGER
    assert elem.arguments[0].data == 5
    assert isinstance(elem.arguments[1], Identifier)
    assert elem.arguments[1] == "True"


def test_parse_function():
    function = parser.parse_statement('generate({"size": get_size()})')
    assert isinstance(function, FunctionCall)
    assert function.type == FunctionType.FUNCTION
    assert function.namespace is None
    assert function.name == "generate"
    assert len(function.arguments) == 1
    arg = function.arguments[0]
    assert arg.type == ObjectTypes.MAP
    arg: ObjectType
    assert len(arg.data) == 1
    pair: ObjectKeyValuePair = arg.data[0]
    key, value = pair.key, pair.value
    assert isinstance(key, StringType)
    assert key.data == "size"
    assert isinstance(value, FunctionCall)
    assert value.type == FunctionType.FUNCTION
    assert value.namespace is None
    assert value.name == "get_size"
    assert len(value.arguments) == 0


def test_parse_identifier():
    parsed = parser.parse_statement("id")
    assert isinstance(parsed, Identifier)
    assert parsed == "id"


def test_parse_value():
    parsed = parser.parse_statement("5.5")
    assert parsed.type == BasicNumericTypes.REAL
    assert math.isclose(parsed.data, 5.5)


def test_parse_cast_set():
    parsed = parser.parse_statement("{} :: set")
    assert parsed.type == BasicSequenceTypes.SET
    assert parsed.data == []
    with pytest.raises(ParseError):
        parser.parse_statement('{"data": "data"} :: set')


def test_parse_property():
    parsed = parser.parse_statement("alpha.beta")
    assert parsed.type == FunctionType.PROPERTY
    assert parsed.arguments == []
    assert parsed.name == "beta"
    assert parsed.namespace == "alpha"


def test_parse_chained_constructor():
    parsed = parser.parse_statement("(new Container(5)).get()")
    assert parsed.type == FunctionType.FUNCTION
    assert parsed.name == "get"
    namespace = parsed.namespace
    assert namespace.namespace is None
    assert namespace.type == FunctionType.CONSTRUCTOR


def test_parse_chained_function():
    parsed = parser.parse_statement("get_container().get()")
    assert parsed.type == FunctionType.FUNCTION
    assert parsed.name == "get"
    namespace = parsed.namespace
    assert namespace.namespace is None
    assert namespace.type == FunctionType.FUNCTION
    assert namespace.name == "get_container"


def test_parse_chained_function2():
    parsed = parser.parse_statement("get_container().get().get()")
    assert parsed.type == FunctionType.FUNCTION
    assert parsed.name == "get"
    namespace = parsed.namespace
    assert namespace.namespace is not None
    assert namespace.type == FunctionType.FUNCTION
    assert namespace.name == "get"
    namespace = namespace.namespace
    assert namespace.namespace is None
    assert namespace.type == FunctionType.FUNCTION
    assert namespace.name == "get_container"


def test_parse_chained_property():
    parsed = parser.parse_statement("get_container().property.data")
    assert parsed.type == FunctionType.PROPERTY
    assert parsed.name == "data"
    namespace = parsed.namespace
    assert namespace.namespace is not None
    assert namespace.type == FunctionType.PROPERTY
    assert namespace.name == "property"
    namespace = namespace.namespace
    assert namespace.namespace is None
    assert namespace.type == FunctionType.FUNCTION
    assert namespace.name == "get_container"


def test_parse_global_variable():
    parsed = parser.parse_statement("<data>")
    assert parsed.type == FunctionType.PROPERTY
    assert parsed.name == "data"
    assert parsed.namespace is None
