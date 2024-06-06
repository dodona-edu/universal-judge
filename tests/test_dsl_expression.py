import math

import pytest

from tested.datatypes import (
    AdvancedNothingTypes,
    AdvancedNumericTypes,
    AdvancedObjectTypes,
    AdvancedSequenceTypes,
    AdvancedStringTypes,
    BasicBooleanTypes,
    BasicNothingTypes,
    BasicNumericTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
    BasicStringTypes,
)
from tested.dsl.ast_translator import InvalidDslError, parse_string
from tested.serialisation import (
    BooleanType,
    FunctionCall,
    FunctionType,
    Identifier,
    NumberType,
    ObjectKeyValuePair,
    ObjectType,
    PropertyAssignment,
    SequenceType,
    StringType,
    VariableAssignment,
    VariableType,
)


def test_parse_value_null():
    parsed = parse_string("None")
    assert parsed.type == BasicNothingTypes.NOTHING
    assert parsed.data is None
    parsed = parse_string("Null")
    assert parsed.type == BasicNothingTypes.NOTHING
    assert parsed.data is None


def test_parse_value_undefined():
    parsed = parse_string("Undefined")
    assert parsed.type == AdvancedNothingTypes.UNDEFINED
    assert parsed.data is None


def test_parse_value_pos_integer():
    parsed = parse_string("5")
    assert parsed.type == BasicNumericTypes.INTEGER
    assert parsed.data == 5


def test_parse_value_neg_integer():
    parsed = parse_string("-10")
    assert parsed.type == BasicNumericTypes.INTEGER
    assert parsed.data == -10


def test_parse_value_integer():
    parsed = parse_string("integer(-10)")
    assert parsed.type == BasicNumericTypes.INTEGER
    assert parsed.data == -10


def test_parse_value_bigint():
    parsed = parse_string("bigint(-1024)")
    assert parsed.type == AdvancedNumericTypes.BIG_INT
    assert parsed.data == -1024


def test_parse_value_uint8():
    parsed = parse_string("uint8(5)")
    assert parsed.type == AdvancedNumericTypes.U_INT_8
    assert parsed.data == 5


def test_parse_value_int8():
    parsed = parse_string("int8(-10)")
    assert parsed.type == AdvancedNumericTypes.INT_8
    assert parsed.data == -10


def test_parse_value_uint16():
    parsed = parse_string("uint16(5)")
    assert parsed.type == AdvancedNumericTypes.U_INT_16
    assert parsed.data == 5


def test_parse_value_int16():
    parsed = parse_string("int16(-10)")
    assert parsed.type == AdvancedNumericTypes.INT_16
    assert parsed.data == -10


def test_parse_value_uint32():
    parsed = parse_string("uint32(5)")
    assert parsed.type == AdvancedNumericTypes.U_INT_32
    assert parsed.data == 5


def test_parse_value_int32():
    parsed = parse_string("int32(-10)")
    assert parsed.type == AdvancedNumericTypes.INT_32
    assert parsed.data == -10


def test_parse_value_uint64():
    parsed = parse_string("uint64(5)")
    assert parsed.type == AdvancedNumericTypes.U_INT_64
    assert parsed.data == 5


def test_parse_value_int64():
    parsed = parse_string("int64(-10)")
    assert parsed.type == AdvancedNumericTypes.INT_64
    assert parsed.data == -10


def test_parse_value_real():
    parsed = parse_string("1.0")
    assert parsed.type == BasicNumericTypes.REAL
    assert math.isclose(parsed.data, 1.0)


def test_parse_value_real_neg():
    parsed = parse_string("real(-1.0)")
    assert parsed.type == BasicNumericTypes.REAL
    assert math.isclose(parsed.data, -1.0)


def test_parse_value_single():
    parsed = parse_string("single_precision(1e-10)")
    assert parsed.type == AdvancedNumericTypes.SINGLE_PRECISION
    assert math.isclose(parsed.data, 1e-10)


def test_parse_value_double():
    parsed = parse_string("double_precision(2.5e+10)")
    assert parsed.type == AdvancedNumericTypes.DOUBLE_PRECISION
    assert math.isclose(parsed.data, 2.5e10)


def test_parse_value_double_extended():
    parsed = parse_string("double_extended(-5.5e5)")
    assert parsed.type == AdvancedNumericTypes.DOUBLE_EXTENDED
    assert math.isclose(parsed.data, -5.5e5)


def test_parse_value_fixed_precision():
    parsed = parse_string("fixed_precision(5.5)")
    assert parsed.type == AdvancedNumericTypes.FIXED_PRECISION
    assert math.isclose(parsed.data, 5.5)


def test_parse_value_true():
    parsed = parse_string("True")
    assert parsed.type == BasicBooleanTypes.BOOLEAN
    assert parsed.data is True


def test_parse_value_false():
    parsed = parse_string("boolean(False)")
    assert parsed.type == BasicBooleanTypes.BOOLEAN
    assert parsed.data is False


def test_parse_value_text():
    parsed = parse_string('"this is a string"')
    assert parsed.type == BasicStringTypes.TEXT
    assert parsed.data == "this is a string"


def test_parse_value_text_cast():
    parsed = parse_string(r'text("this\nis\na\nstring")')
    assert parsed.type == BasicStringTypes.TEXT
    assert parsed.data == "this\nis\na\nstring"


def test_parse_value_char():
    parsed = parse_string('char("c")')
    assert parsed.type == AdvancedStringTypes.CHAR
    assert parsed.data == "c"


def test_parse_value_sequence():
    parsed = parse_string('[5, "text"]')
    assert parsed.type == BasicSequenceTypes.SEQUENCE
    assert len(parsed.data) == 2
    assert parsed.data[0].type == BasicNumericTypes.INTEGER
    assert parsed.data[0].data == 5
    assert parsed.data[1].type == BasicStringTypes.TEXT
    assert parsed.data[1].data == "text"


def test_parse_value_set():
    parsed = parse_string('{char("d"), uint8(8)}')
    assert parsed.type == BasicSequenceTypes.SET
    assert len(parsed.data) == 2
    assert parsed.data[0].type == AdvancedStringTypes.CHAR
    assert parsed.data[0].data == "d"
    assert parsed.data[1].type == AdvancedNumericTypes.U_INT_8
    assert parsed.data[1].data == 8


def test_parse_value_tuple():
    parsed = parse_string("(True, False)")
    assert parsed.type == AdvancedSequenceTypes.TUPLE
    assert len(parsed.data) == 2
    assert parsed.data[0].type == BasicBooleanTypes.BOOLEAN
    assert parsed.data[0].data is True
    assert parsed.data[1].type == BasicBooleanTypes.BOOLEAN
    assert parsed.data[1].data is False


def test_parse_value_adv_sequence():
    parsed = parse_string('list([5, array(["text", "data"])])')
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
    parsed = parse_string('{"ignore": True, 5: 0}')
    assert parsed.type == BasicObjectTypes.MAP
    parsed: ObjectType
    assert len(parsed.data) == 2
    key, value = parsed.data[0].key, parsed.data[0].value
    assert isinstance(key, StringType)
    assert key.type == BasicStringTypes.TEXT
    assert key.data == "ignore"
    assert isinstance(value, BooleanType)
    assert value.type == BasicBooleanTypes.BOOLEAN
    assert value.data is True
    key, value = parsed.data[1].key, parsed.data[1].value
    assert isinstance(key, NumberType)
    assert key.type == BasicNumericTypes.INTEGER
    assert key.data == 5
    assert isinstance(value, NumberType)
    assert value.type == BasicNumericTypes.INTEGER
    assert value.data == 0


def test_parse_value_dictionary():
    parsed = parse_string('dictionary({"ignore": True, 5: 0})')
    assert parsed.type == AdvancedObjectTypes.DICTIONARY
    parsed: ObjectType
    assert len(parsed.data) == 2
    key, value = parsed.data[0].key, parsed.data[0].value
    assert isinstance(key, StringType)
    assert key.type == BasicStringTypes.TEXT
    assert key.data == "ignore"
    assert isinstance(value, BooleanType)
    assert value.type == BasicBooleanTypes.BOOLEAN
    assert value.data is True
    key, value = parsed.data[1].key, parsed.data[1].value
    assert isinstance(key, NumberType)
    assert key.type == BasicNumericTypes.INTEGER
    assert key.data == 5
    assert isinstance(value, NumberType)
    assert value.type == BasicNumericTypes.INTEGER
    assert value.data == 0


def test_parse_value_object():
    parsed = parse_string('object({"ignore": True, 5: 0})')
    assert parsed.type == AdvancedObjectTypes.OBJECT
    parsed: ObjectType
    assert len(parsed.data) == 2
    key, value = parsed.data[0].key, parsed.data[0].value
    assert isinstance(key, StringType)
    assert key.type == BasicStringTypes.TEXT
    assert key.data == "ignore"
    assert isinstance(value, BooleanType)
    assert value.type == BasicBooleanTypes.BOOLEAN
    assert value.data is True
    key, value = parsed.data[1].key, parsed.data[1].value
    assert isinstance(key, NumberType)
    assert key.type == BasicNumericTypes.INTEGER
    assert key.data == 5
    assert isinstance(value, NumberType)
    assert value.type == BasicNumericTypes.INTEGER
    assert value.data == 0


def test_parse_error_fun_in_return_value():
    with pytest.raises(InvalidDslError):
        parse_string("[fun()]", is_return=True)


def test_parse_error_property_in_return_value():
    with pytest.raises(InvalidDslError):
        parse_string("{data.data}", is_return=True)


def test_parse_error_constructor_in_return_value():
    with pytest.raises(InvalidDslError):
        parse_string('{"data": data.Object()}', is_return=True)


def test_parse_error_constructor_in_return_value2():
    with pytest.raises(InvalidDslError):
        parse_string('{data.Object(): "data"}', is_return=True)


def test_syntax_error_in_parser():
    with pytest.raises(InvalidDslError):
        parse_string("<test>")


def test_parse_error_fun_assign():
    with pytest.raises(InvalidDslError):
        parse_string("data = first([object.gen_int()])")


def test_parse_fun_assign():
    assign = parse_string("data: integer = first([object.gen_int()])")
    assert isinstance(assign, VariableAssignment)
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
    assign = parse_string("cont: Container = Container({object.version})")
    assert isinstance(assign, VariableAssignment)
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
    assign = parse_string("cont = Container({object.version})")
    assert isinstance(assign, VariableAssignment)
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
    assign = parse_string("lijst: list = list([Container(5, true)])")
    assert isinstance(assign, VariableAssignment)
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
    assert elem.arguments[1] == "true"


def test_parse_attribute_assign():
    assign = parse_string("the_object.data = first([object.gen_int()])")
    assert isinstance(assign, PropertyAssignment)
    assert assign.property.type == FunctionType.PROPERTY
    assert assign.property.name == "data"
    assert assign.property.namespace == "the_object"
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


def test_parse_attribute_assign_hinted():
    with pytest.raises(InvalidDslError):
        parse_string("the_object.data: Test = first([object.gen_int()])")


def test_parse_function():
    function = parse_string('generate({"size": get_size()})')
    assert isinstance(function, FunctionCall)
    assert function.type == FunctionType.FUNCTION
    assert function.namespace is None
    assert function.name == "generate"
    assert len(function.arguments) == 1
    arg = function.arguments[0]
    assert arg.type == BasicObjectTypes.MAP
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


def test_parse_raw_capitalization_function():
    function = parse_string("__TheFunction__(5)")
    assert isinstance(function, FunctionCall)
    assert function.type == FunctionType.FUNCTION
    assert function.namespace is None
    assert function.name == "TheFunction"
    assert len(function.arguments) == 1
    arg = function.arguments[0]
    assert arg.type == BasicNumericTypes.INTEGER


def test_parse_raw_capitalization_property():
    function = parse_string("__test__.__some_class__")
    assert isinstance(function, FunctionCall)
    assert function.type == FunctionType.PROPERTY
    assert function.namespace == "test"
    assert function.name == "some_class"


def test_parse_raw_capitalization_variable():
    assign = parse_string("__test__ = 5")
    assert isinstance(assign, VariableAssignment)
    assert assign.variable == "test"


def test_parse_identifier():
    parsed = parse_string("id")
    assert isinstance(parsed, Identifier)
    assert parsed == "id"


def test_parse_value():
    parsed = parse_string("5.5")
    assert parsed.type == BasicNumericTypes.REAL
    assert math.isclose(parsed.data, 5.5)


def test_parse_cast_set():
    with pytest.raises(InvalidDslError):
        parse_string("set({})")
    with pytest.raises(InvalidDslError):
        parse_string('set({"data": "data"})')


def test_parse_property():
    parsed = parse_string("alpha.beta")
    assert parsed.type == FunctionType.PROPERTY
    assert parsed.arguments == []
    assert parsed.name == "beta"
    assert parsed.namespace == "alpha"


def test_parse_chained_constructor():
    parsed = parse_string("Container(5).get()")
    assert parsed.type == FunctionType.FUNCTION
    assert parsed.name == "get"
    namespace = parsed.namespace
    assert namespace.namespace is None
    assert namespace.type == FunctionType.CONSTRUCTOR


def test_parse_chained_function():
    parsed = parse_string("get_container().get()")
    assert parsed.type == FunctionType.FUNCTION
    assert parsed.name == "get"
    namespace = parsed.namespace
    assert namespace.namespace is None
    assert namespace.type == FunctionType.FUNCTION
    assert namespace.name == "get_container"


def test_parse_chained_function2():
    parsed = parse_string("get_container().get().get()")
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
    parsed = parse_string("get_container().property.data")
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
