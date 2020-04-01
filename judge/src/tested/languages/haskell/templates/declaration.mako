## Convert a Value to a type.
<%! from tested.serialisation import VariableType, as_basic_type, resolve_to_basic %>
<%! from tested.datatypes import AdvancedNumericTypes, AdvancedSequenceTypes  %>
<%! from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, BasicNothingTypes, BasicSequenceTypes, BasicObjectTypes  %>
<%page args="tp" />
% if isinstance(tp, VariableType):
    ${tp.data}
% elif tp == AdvancedSequenceTypes.TUPLE:
    \
% elif tp == AdvancedNumericTypes.U_INT_64:
    Data.Word.Word64\
% elif tp == AdvancedNumericTypes.U_INT_32:
    Data.Word.Word32\
% elif tp == AdvancedNumericTypes.U_INT_16:
    Data.Word.Word16\
% elif tp == AdvancedNumericTypes.U_INT_8:
    Data.Word.Word8\
% elif tp == AdvancedNumericTypes.INT_64:
    Data.Int.Int64\
% elif tp == AdvancedNumericTypes.INT_32:
    Data.Int.Int32\
% elif tp == AdvancedNumericTypes.INT_16:
    Data.Int.Int16\
% elif tp == AdvancedNumericTypes.INT_8:
    Data.Int.Int8\
% elif tp == AdvancedNumericTypes.SINGLE_PRECISION:
    Float\
% elif tp == AdvancedNumericTypes.DOUBLE_PRECISION:
    Double\
% else:
    <% basic = resolve_to_basic(tp) %>
    % if basic == BasicBooleanTypes.BOOLEAN:
        Bool\
    % elif basic == BasicStringTypes.TEXT:
        String\
    % elif basic == BasicStringTypes.CHAR:
        Char\
    % elif basic == BasicNumericTypes.INTEGER:
        Integer\
    % elif basic == BasicNumericTypes.RATIONAL:
        Double\
    % elif basic == BasicNothingTypes.NOTHING:
        Nothing\
    % endif
% endif
