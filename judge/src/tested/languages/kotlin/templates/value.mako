## Convert a Value to a literal type in Kotlin.
<%! from tested.datatypes import AdvancedSequenceTypes, AdvancedNumericTypes %>\
<%! from tested.serialisation import as_basic_type %>\
<%page args="value" />\
## First, add support for the advanced types in Kotlin.
% if value.type == AdvancedSequenceTypes.ARRAY:
    arrayOf(<%include file="value_arguments.mako" args="arguments=value.data"/>)
% elif value.type in (AdvancedNumericTypes.U_INT_64, AdvancedNumericTypes.BIG_INT):
    BigInteger("${data.value}")
% elif value.type in (AdvancedNumericTypes.DOUBLE_EXTENDED, AdvancedNumericTypes.FIXED_PRECISION):
    BigDecimal("${data.value}")
% else:
    ## Handle the base types
    <% basic = as_basic_type(value) %>\
    <%include file="value_basic.mako" args="value=basic" />\
% endif