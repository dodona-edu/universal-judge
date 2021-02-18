## Convert a Value to a type.
<%! from tested.utils import get_args %>\
<%! from tested.serialisation import VariableType, as_basic_type, resolve_to_basic, Value %>\
<%! from tested.datatypes import AdvancedNumericTypes, AdvancedSequenceTypes  %>\
<%! from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>\
<%page args="tp,value" />\
% if isinstance(tp, VariableType):
    ${tp.data}?\
% elif tp == AdvancedSequenceTypes.ARRAY:
    <% type_ = value.get_content_type() %>\
    <% assert value is not None, "Value is needed for arrays!" %>\
    Array<<%include file="declaration.mako" args="tp=type_,value=None"/>>?\
% elif tp in (AdvancedNumericTypes.U_INT_64, AdvancedNumericTypes.BIG_INT):
    BigInteger?\
% elif tp in (AdvancedNumericTypes.DOUBLE_EXTENDED, AdvancedNumericTypes.FIXED_PRECISION):
    BigDecimal?\
% elif tp == AdvancedNumericTypes.INT_8:
    Byte?\
% elif tp in (AdvancedNumericTypes.U_INT_8, AdvancedNumericTypes.INT_16):
    Short?\
% elif tp in (AdvancedNumericTypes.U_INT_16, AdvancedNumericTypes.INT_32):
    Int?\
% elif tp in (AdvancedNumericTypes.U_INT_32, AdvancedNumericTypes.INT_64):
    Long?\
% elif tp == AdvancedNumericTypes.SINGLE_PRECISION:
    Float?\
% elif tp == "Object":
    Any?\
% else:
    <% basic = resolve_to_basic(tp) %>\
    % if basic == BasicSequenceTypes.SEQUENCE:
        <% type_ = (value.get_content_type() or "Object") if isinstance(value, get_args(Value)) else "Object" %>\
        List<<%include file="declaration.mako" args="tp=type_,value=None"/>>?\
    % elif basic == BasicSequenceTypes.SET:
        <% type_ = (value.get_content_type() or "Object") if isinstance(value, get_args(Value)) else "Object" %>\
        Set<<%include file="declaration.mako" args="tp=type_,value=None"/>>?\
    % elif basic == BasicBooleanTypes.BOOLEAN:
        Boolean?\
    % elif basic == BasicStringTypes.TEXT:
        String?\
    % elif basic == BasicStringTypes.CHAR:
        Char?\
    % elif basic == BasicNumericTypes.INTEGER:
        Long?\
    % elif basic == BasicNumericTypes.RATIONAL:
        Double?\
    % elif basic == BasicObjectTypes.MAP:
        Map${'<String, Any?>?'}\
    % elif basic in (BasicNothingTypes.NOTHING, BasicStringTypes.ANY):
        Any?\
    % endif
% endif