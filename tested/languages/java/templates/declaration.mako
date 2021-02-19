## Convert a Value to a type.
<%! from tested.utils import get_args %>\
<%! from tested.serialisation import VariableType, as_basic_type, resolve_to_basic, Value %>\
<%! from tested.datatypes import AdvancedNumericTypes, AdvancedSequenceTypes  %>\
<%! from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>\
<%page args="tp,value,inner=False" />\
% if isinstance(tp, VariableType):
    ${tp.data}\
% elif tp == AdvancedSequenceTypes.ARRAY:
    <% type_ = value.get_content_type() %>\
    <% assert value is not None, "Value is needed for arrays!" %>\
    <%include file="declaration.mako" args="tp=type_,value=None"/>[]\
% elif tp in (AdvancedNumericTypes.U_INT_64, AdvancedNumericTypes.BIG_INT):
    BigInteger\
% elif tp in (AdvancedNumericTypes.DOUBLE_EXTENDED, AdvancedNumericTypes.FIXED_PRECISION):
    BigDecimal\
% elif tp == AdvancedNumericTypes.INT_8:
    %if inner:
        Byte\
    %else:
        byte\
    %endif
% elif tp in (AdvancedNumericTypes.U_INT_8, AdvancedNumericTypes.INT_16):
    %if inner:
        Short\
    %else:
        short\
    %endif
% elif tp in (AdvancedNumericTypes.U_INT_16, AdvancedNumericTypes.INT_32):
    %if inner:
        Integer\
    %else:
        int\
    %endif
% elif tp in (AdvancedNumericTypes.U_INT_32, AdvancedNumericTypes.INT_64):
    %if inner:
        Long\
    %else:
        long\
    %endif
    long\
% elif tp == AdvancedNumericTypes.SINGLE_PRECISION:
    %if inner:
        Float\
    %else:
        float\
    %endif
% elif tp == "Object":
    Object\
% else:
    <% basic = resolve_to_basic(tp) %>\
    % if basic == BasicSequenceTypes.SEQUENCE:
        <% type_ = (value.get_content_type() or "Object") if isinstance(value, get_args(Value)) else "Object" %>\
        List<<%include file="declaration.mako" args="tp=type_,value=None,inner=True"/>>\
    % elif basic == BasicSequenceTypes.SET:
        <% type_ = (value.get_content_type() or "Object") if isinstance(value, get_args(Value)) else "Object" %>\
        Set<<%include file="declaration.mako" args="tp=type_,value=None,inner=True"/>>\
    % elif basic == BasicBooleanTypes.BOOLEAN:
        %if inner:
            Boolean\
        %else:
            boolean\
        %endif
    % elif basic == BasicStringTypes.TEXT:
        String\
    % elif basic == BasicStringTypes.CHAR:
        %if inner:
            Character\
        %else:
            char\
        %endif
    % elif basic == BasicNumericTypes.INTEGER:
        %if inner:
            Long\
        %else:
            long\
        %endif
    % elif basic == BasicNumericTypes.RATIONAL:
        %if inner:
            Double\
        %else:
            double\
        %endif
    % elif basic == BasicObjectTypes.MAP:
        <% key_type_ = (value.get_key_type() or "Object") if isinstance(value, get_args(Value)) else "Object" %>\
        <% value_type_ = (value.get_value_type() or "Object") if isinstance(value, get_args(Value)) else "Object" %>\
        Map<<%include file="declaration.mako" args="tp=key_type_,value=None,inner=True"/>, \
        <%include file="declaration.mako" args="tp=value_type_,value=None,inner=True"/>>'\
    % elif basic in (BasicNothingTypes.NOTHING, BasicStringTypes.ANY):
        Object\
    % endif
% endif