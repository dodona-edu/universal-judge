
from tested.datatypes import AllTypes, resolve_to_basic
from tested.datatypes.advanced import AdvancedObjectTypes, AdvancedSequenceTypes, AdvancedStringTypes
from tested.datatypes.basic import BasicObjectTypes, BasicSequenceTypes, BasicStringTypes, BasicTypes
from tested.languages.c.generators import CGenerator
from tested.languages.preparation import PreparedExecutionUnit, PreparedFunctionCall, PreparedTestcase, PreparedTestcaseStatement
from tested.serialisation import FunctionCall, FunctionType, ObjectType, PropertyAssignment, SequenceType, Statement, Value, VariableAssignment, VariableType, WrappedAllTypes


class CPPGenerator(CGenerator):
    def  unpack_wrapped_types(self, type_or_types: WrappedAllTypes) -> tuple[AllTypes, WrappedAllTypes]:
        if isinstance(type_or_types, tuple):
            return type_or_types
        return type_or_types, None

    def convert_sequence_subtype(self, value: Statement, subtype: AllTypes) -> str:
        if value and isinstance(value, SequenceType):
            # if the value is a sequence, we need to know the types of it's elements
            type_or_types = value.get_content_type()
        elif subtype:
            # we might already have a subtype extracted from a previous recursive call
            type_or_types = subtype
        else:
            # c++ has no default type such as Object in java, so we can't infer the type
            return None

        tp, subtype = self.unpack_wrapped_types(type_or_types)
        return self.convert_declaration(tp, None, subtype)
    
    def convert_map_subtypes(self, value: Statement, subtype: WrappedAllTypes) -> tuple[str, str] | None:
        if isinstance(value, ObjectType):
            key_type = value.get_key_type()
            value_type = value.get_value_type()
        elif subtype:
            key_type, value_type = subtype
        else:
            return None
        key_base_type, key_sub_type = self.extract_type_tuple(key_type)
        value_base_type, value_sub_type = self.extract_type_tuple(value_type)
        key_type_str = self.convert_declaration(key_base_type, None, key_sub_type)
        value_type_str = self.convert_declaration(value_base_type, None, value_sub_type)

        return key_type_str, value_type_str
    
    def convert_value(self, value: Value) -> str:
        tp = value.type
        basic = resolve_to_basic(tp)
        if basic == BasicObjectTypes.MAP:
            return "{" + ", ".join(f"{self.convert_value(k), self.convert_value(v)}" for k, v in value.data.items()) + "}"
        elif basic == BasicSequenceTypes.SEQUENCE or basic == BasicSequenceTypes.SET:
            return "{" + ", ".join(self.convert_value(v) for v in value.data) + "}"
        elif basic == BasicStringTypes.TEXT:
            return f'std::string("{value.data}")'

        return super().convert_value(value)
        


    def convert_declaration(self, tp: AllTypes | VariableType, 
                            value: Statement | None = None,
                            subtype: WrappedAllTypes| None = None) -> str:
        if isinstance(tp, VariableType):
            return tp.data + "*"
        if tp == AdvancedSequenceTypes.LIST:
            subtype = self.convert_sequence_subtype(value, subtype)
            return f"std::list<{subtype}>"
        elif tp == AdvancedSequenceTypes.TUPLE:
            # this method does not support tuples within sequences such as list<tuple<int, int>>
            # as value won't be defined in that case and we cant't infer the tuple's length
            # we also don't support tuples with different types, as we can only extract one type
            assert value is not None and isinstance(value, SequenceType)
            tuple_length = len(value.data)
            subtype = self.convert_sequence_subtype(value, subtype)
            return f"std::tuple<{", ".join(subtype for _ in range(tuple_length))}>"
        elif tp == AdvancedSequenceTypes.ARRAY:
            subtype = self.convert_sequence_subtype(value, subtype)
            return f"std::vector<{subtype}>"
        elif tp == AdvancedStringTypes.STRING:
            return "std::string"
        
        basic = resolve_to_basic(tp)
        if basic == BasicObjectTypes.MAP:
            key_type, value_type = self.convert_map_subtypes(value, subtype)
            return f"std::map<{key_type}, {value_type}>"
        elif basic == BasicSequenceTypes.SET:
            subtype = self.convert_sequence_subtype(value, subtype)
            return f"std::set<{subtype}>"
        elif basic == BasicSequenceTypes.SEQUENCE:
            subtype = self.convert_sequence_subtype(value, subtype)
            return f"std::vector<{subtype}>"
        elif basic == BasicStringTypes.TEXT:
            return "std::string"
        elif basic == BasicStringTypes.ANY:
            return "std::any"


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
        if (function.namespace 
            and not function.has_root_namespace
            and not function.type == FunctionType.CONSTRUCTOR
            ):
            result = self.convert_statement(function.namespace) + "->" + result
        # add the new keyword to constructors
        if function.type == FunctionType.CONSTRUCTOR:
            result = "new " + result
        return result
    
    def convert_testcase(self, tc: PreparedTestcase, pu: PreparedExecutionUnit) -> str:
        result = ""
        # Define variables before asignment
        if ( not tc.testcase.is_main_testcase()
             and isinstance(tc.input, PreparedTestcaseStatement)
             and isinstance(tc.input.statement, VariableAssignment)
        ):
            prefix = self.convert_declaration(tc.input.statement.type, tc.input.statement.expression)
            result += f"{prefix} {tc.input.statement.variable};\n"
        
        result += super().convert_testcase(tc, pu)
        return result
