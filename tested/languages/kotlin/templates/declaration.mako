## Convert a Value to a type.
<%! from tested.utils import get_args %>\
<%! from tested.serialisation import VariableType, as_basic_type, resolve_to_basic, Value %>\
<%! from tested.datatypes import AdvancedNumericTypes, AdvancedSequenceTypes  %>\
<%! from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>\
<%page args="tp,value,nt=None,inner=False" />\
<%!
    def extract_type_tuple(type_):
        if isinstance(type_, tuple):
            return type_
        else:
            return type_, False
%>\
% if isinstance(tp, VariableType):
    ${tp.data}?\
% elif tp == AdvancedSequenceTypes.ARRAY:
    <% type_ = (value.get_content_type() or "Object") if isinstance(value, get_args(Value)) else nt if nt else 'Object' %>\
    <% base_type, sub_type = extract_type_tuple(type_) %>\
    Array<<%include file="declaration.mako" args="tp=base_type,value=None,nt=sub_type,inner=True"/>>?\
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
        <% type_ = (value.get_content_type() or "Object") if isinstance(value, get_args(Value)) else nt if nt else 'Object' %>\
        <% base_type, sub_type = extract_type_tuple(type_) %>\
        List<<%include file="declaration.mako" args="tp=base_type,value=None,nt=sub_type,inner=True"/>>?\
    % elif basic == BasicSequenceTypes.SET:
        <% type_ = (value.get_content_type() or "Object") if isinstance(value, get_args(Value)) else nt if nt else 'Object' %>\
        <% base_type, sub_type = extract_type_tuple(type_) %>\
        Set<<%include file="declaration.mako" args="tp=base_type,value=None,nt=sub_type,inner=True"/>>\
    % elif basic == BasicBooleanTypes.BOOLEAN:
        Boolean?\
    % elif basic == BasicStringTypes.TEXT:
        String?\
    % elif basic == BasicStringTypes.CHAR:
        Char?\
    % elif basic == BasicNumericTypes.INTEGER:
        Int?\
    % elif basic == BasicNumericTypes.RATIONAL:
        Double?\
    % elif basic == BasicObjectTypes.MAP:
        % if isinstance(value, get_args(Value)):
            <% key_type_ = value.get_key_type() or "Object" %>\
            <% value_type_ = value.get_value_type() or "Object" %>\
        % elif nt:
            <% key_type_, value_type_ = nt %>\
        % else:
            <% key_type_, value_type_ = "Object", "Object" %>\
        % endif
        <% key_base_type, key_sub_type = extract_type_tuple(key_type_) %>\
        <% value_base_type, value_sub_type = extract_type_tuple(value_type_) %>\
        Map<<%include file="declaration.mako" args="tp=key_base_type,value=None,nt=key_sub_type,inner=True"/>, \
        <%include file="declaration.mako" args="tp=value_base_type,value=None,nt=value_sub_type,inner=True"/>>?\
    % elif basic in (BasicNothingTypes.NOTHING, BasicStringTypes.ANY):
        Any?\
    % endif
% endif