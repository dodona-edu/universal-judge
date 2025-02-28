from typing import cast

from tested.datatypes import AllTypes, resolve_to_basic
from tested.datatypes.advanced import (
    AdvancedNumericTypes,
    AdvancedSequenceTypes,
    AdvancedStringTypes,
)
from tested.datatypes.basic import (
    BasicNothingTypes,
    BasicNumericTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
    BasicStringTypes,
)
from tested.languages.c.generators import CGenerator
from tested.languages.preparation import (
    PreparedExecutionUnit,
    PreparedFunctionCall,
    PreparedTestcase,
    PreparedTestcaseStatement,
)
from tested.serialisation import (
    FunctionCall,
    FunctionType,
    ObjectType,
    PropertyAssignment,
    SequenceType,
    Statement,
    Value,
    VariableAssignment,
    VariableType,
    WrappedAllTypes,
)

type Subtype = WrappedAllTypes | tuple[WrappedAllTypes, WrappedAllTypes] | None


class CPPGenerator(CGenerator):
    def unpack_wrapped_types(
        self, type_or_types: WrappedAllTypes
    ) -> tuple[AllTypes, Subtype]:
        if isinstance(type_or_types, tuple):
            return type_or_types
        return type_or_types, None

    def convert_sequence_subtype(
        self, value: Statement | None, subtype: Subtype
    ) -> str | None:
        if value and isinstance(value, SequenceType):
            # if the value is a sequence, we need to know the types of it's elements
            type_or_types = value.get_content_type()
        elif subtype:
            # we might already have a subtype extracted from a previous recursive call
            type_or_types = cast(WrappedAllTypes, subtype)
        else:
            # c++ has no default type such as Object in java, so we can't infer the type
            return None

        tp, new_subtype = self.unpack_wrapped_types(type_or_types)
        return self.convert_declaration(tp, None, new_subtype)

    def convert_map_subtypes(
        self, value: Statement | None, subtype: Subtype
    ) -> tuple[str, str] | None:
        if value and isinstance(value, ObjectType):
            key_type = value.get_key_type()
            value_type = value.get_value_type()
        elif subtype:
            key_type, value_type = cast(
                tuple[WrappedAllTypes, WrappedAllTypes], subtype
            )
        else:
            return None
        key_base_type, key_sub_type = self.unpack_wrapped_types(key_type)
        value_base_type, value_sub_type = self.unpack_wrapped_types(value_type)
        key_type_str = self.convert_declaration(key_base_type, None, key_sub_type)
        value_type_str = self.convert_declaration(value_base_type, None, value_sub_type)

        return key_type_str, value_type_str

    def convert_value(self, value: Value) -> str:
        tp = value.type
        basic = resolve_to_basic(tp)
        if basic == BasicObjectTypes.MAP:
            return (
                "{"
                + ", ".join(
                    f"{self.convert_value(cast(Value, kvp.key)), self.convert_value(cast(Value, kvp.value))}"
                    for kvp in cast(ObjectType, value).data
                )
                + "}"
            )
        elif basic == BasicSequenceTypes.SEQUENCE or basic == BasicSequenceTypes.SET:
            return (
                "{"
                + ", ".join(
                    self.convert_value(cast(Value, v))
                    for v in cast(SequenceType, value).data
                )
                + "}"
            )
        elif value.type == BasicNothingTypes.NOTHING:
            return ""

        return super().convert_value(value)

    def convert_declaration(
        self,
        tp: AllTypes | VariableType,
        value: Statement | None = None,
        subtype: Subtype = None,
    ) -> str:
        if isinstance(tp, VariableType):
            return tp.data + "*"
        elif tp == AdvancedNumericTypes.BIG_INT:
            return "std::intmax_t"
        elif tp == AdvancedNumericTypes.U_INT_64:
            return "std::uint64_t"
        elif tp == AdvancedNumericTypes.INT_64:
            return "std::int64_t"
        elif tp == AdvancedNumericTypes.U_INT_32:
            return "std::uint32_t"
        elif tp == AdvancedNumericTypes.INT_32:
            return "std::int32_t"
        elif tp == AdvancedNumericTypes.U_INT_16:
            return "std::uint16_t"
        elif tp == AdvancedNumericTypes.INT_16:
            return "std::int16_t"
        elif tp == AdvancedNumericTypes.U_INT_8:
            return "std::uint8_t"
        elif tp == AdvancedNumericTypes.INT_8:
            return "std::int8_t"
        if tp == AdvancedSequenceTypes.LIST:
            subtype_string = self.convert_sequence_subtype(value, subtype)
            return f"std::list<{subtype_string}>"
        elif tp == AdvancedSequenceTypes.TUPLE:
            # this method does not support tuples within sequences such as list<tuple<int, int>>
            # as value won't be defined in that case and we cant't infer the tuple's length
            # we also don't support tuples with different types, as we can only extract one type
            assert value is not None and isinstance(value, SequenceType)
            tuple_length = len(value.data)
            subtype_string = self.convert_sequence_subtype(value, subtype)
            assert subtype_string is not None
            return (
                f"std::tuple<{", ".join(subtype_string for _ in range(tuple_length))}>"
            )
        elif tp == AdvancedSequenceTypes.ARRAY:
            subtype_string = self.convert_sequence_subtype(value, subtype)
            return f"std::vector<{subtype_string}>"
        elif tp == AdvancedStringTypes.STRING:
            return "std::string"

        basic = resolve_to_basic(tp)
        if basic == BasicObjectTypes.MAP:
            subtype_strings = self.convert_map_subtypes(value, subtype)
            if subtype_strings is None:
                return "std::map<>"
            key_type, value_type = subtype_strings
            return f"std::map<{key_type}, {value_type}>"
        elif basic == BasicSequenceTypes.SET:
            subtype_string = self.convert_sequence_subtype(value, subtype)
            return f"std::set<{subtype_string}>"
        elif basic == BasicSequenceTypes.SEQUENCE:
            subtype_string = self.convert_sequence_subtype(value, subtype)
            return f"std::vector<{subtype_string}>"
        elif basic == BasicStringTypes.TEXT:
            return "std::string"
        elif basic == BasicStringTypes.ANY:
            return "std::any"
        elif basic == BasicNumericTypes.INTEGER:
            return "std::intmax_t"

        return super().convert_declaration(tp)

    def convert_statement(self, statement: Statement, full=False) -> str:
        # support for property assignments
        if isinstance(statement, PropertyAssignment):
            return (
                f"{self.convert_statement(statement.property)} = "
                f"{self.convert_statement(statement.expression)};"
            )
        # overwrite the default implementation for variable assignments to allow for
        # object declarations
        elif full and isinstance(statement, VariableAssignment):

            prefix = self.convert_declaration(statement.type, statement.expression)
            return (
                f"{prefix} {statement.variable} = "
                f"{self.convert_statement(statement.expression)}"
            )

        return super().convert_statement(statement, full)

    def convert_function_call(self, function: FunctionCall) -> str:
        result = super().convert_function_call(function)

        # if the function has a namespace, that is not the root namespace we assume it is a method call
        if (
            function.namespace
            and not (
                isinstance(function, PreparedFunctionCall)
                and function.has_root_namespace
            )
            and not function.type == FunctionType.CONSTRUCTOR
        ):
            result = self.convert_statement(function.namespace) + "->" + result
        # add the new keyword to constructors
        if function.type == FunctionType.CONSTRUCTOR:
            result = "new " + result
        return result

    def convert_testcase(self, tc: PreparedTestcase, pu: PreparedExecutionUnit) -> str:
        result = ""
        # Define variables before asignment outside the try block
        if (
            not tc.testcase.is_main_testcase()
            and isinstance(tc.input, PreparedTestcaseStatement)
            and isinstance(tc.input.statement, VariableAssignment)
        ):
            prefix = self.convert_declaration(
                tc.input.statement.type, tc.input.statement.expression
            )
            result += f"{prefix} {tc.input.statement.variable};\n"

        # catch exceptions and write them to the output
        result += "try {" + "\n"
        result += super().convert_testcase(tc, pu)
        result += "\n} catch (std::exception_ptr e) {\n"
        result += self.convert_statement(tc.exception_statement("e")) + ";\n"
        result += "exit_code = 1;\n"
        result += "}\n"
        return result

    def define_write_funtions(self, pu: PreparedExecutionUnit) -> str:
        result = super().define_write_funtions(pu)

        # add a write function for exceptions
        result += f"""
        #undef send_exception
        #define send_exception(value) write_exception({pu.unit.name}_value_file, value)
        """
        return result
