import json

from tested.datatypes import (
    AdvancedNumericTypes,
    AdvancedSequenceTypes,
    AdvancedStringTypes,
    AllTypes,
    BasicBooleanTypes,
    BasicNothingTypes,
    BasicNumericTypes,
    BasicSequenceTypes,
    BasicStringTypes,
    resolve_to_basic,
)
from tested.languages.preparation import (
    PreparedContext,
    PreparedExecutionUnit,
    PreparedTestcase,
    PreparedTestcaseStatement,
)
from tested.languages.utils import convert_unknown_type
from tested.serialisation import (
    Expression,
    FunctionCall,
    Identifier,
    NamedArgument,
    SequenceType,
    SpecialNumbers,
    Statement,
    StringType,
    Value,
    VariableAssignment,
    VariableType,
    as_basic_type,
)
from tested.testsuite import MainInput
from tested.utils import is_statement_strict


def convert_arguments(arguments: list[Expression]) -> str:
    return ", ".join(convert_statement(arg) for arg in arguments)


def convert_value(value: Value) -> str:
    # Handle some advanced types.
    if value.type == AdvancedSequenceTypes.TUPLE:
        assert isinstance(value, SequenceType)
        return f"({convert_arguments(value.data)})"
    elif isinstance(value.type, AdvancedNumericTypes):
        if not isinstance(value.data, SpecialNumbers):
            return f"{value.data} :: {convert_declaration(value.type)}"
        elif value.data == SpecialNumbers.NOT_A_NUMBER:
            return f"(0/0) :: {convert_declaration(value.type)}"
        elif value.data == SpecialNumbers.POS_INFINITY:
            return f"(1/0) :: {convert_declaration(value.type)}"
        else:
            assert value.data == SpecialNumbers.NEG_INFINITY
            return f"(-1/0) :: {convert_declaration(value.type)}"
    elif value.type == AdvancedStringTypes.CHAR:
        assert isinstance(value, StringType)
        return "'" + value.data.replace("'", "\\'") + "'"
    # Handle basic types
    value = as_basic_type(value)
    if value.type == BasicNumericTypes.INTEGER:
        return f"{value.data} :: Int"
    elif value.type == BasicNumericTypes.REAL:
        if not isinstance(value.data, SpecialNumbers):
            return f"{value.data} :: Double"
        elif value.data == SpecialNumbers.NOT_A_NUMBER:
            return "(0/0) :: Double"
        elif value.data == SpecialNumbers.POS_INFINITY:
            return "(1/0) :: Double"
        else:
            assert SpecialNumbers.NEG_INFINITY
            return "(-1/0) :: Double"
    elif value.type == BasicStringTypes.TEXT:
        return json.dumps(value.data)
    elif value.type == BasicBooleanTypes.BOOLEAN:
        return str(value.data)
    elif value.type == BasicNothingTypes.NOTHING:
        return "Nothing :: Maybe Integer"
    elif value.type == BasicSequenceTypes.SEQUENCE:
        assert isinstance(value, SequenceType)
        return f"[{convert_arguments(value.data)}]"
    elif value.type == BasicStringTypes.UNKNOWN:
        assert isinstance(value, StringType)
        return convert_unknown_type(value)
    raise AssertionError(f"Invalid literal: {value!r}")


def convert_function_call(function: FunctionCall) -> str:
    result = ""
    if function.namespace:
        result += convert_statement(function.namespace) + "."
    result += function.name + " "
    for i, argument in enumerate(function.arguments):
        if isinstance(argument, Value):
            result += convert_statement(argument)
        else:
            assert not isinstance(argument, NamedArgument)
            result += "(" + convert_statement(argument) + ")"
        if i != len(function.arguments) - 1:
            result += " "
    return result


def convert_declaration(tp: AllTypes | VariableType) -> str:
    if isinstance(tp, VariableType):
        return tp.data
    elif tp == AdvancedNumericTypes.U_INT_64:
        return "Data.Word.Word64"
    elif tp == AdvancedNumericTypes.U_INT_32:
        return "Data.Word.Word32"
    elif tp == AdvancedNumericTypes.U_INT_16:
        return "Data.Word.Word16"
    elif tp == AdvancedNumericTypes.U_INT_8:
        return "Data.Word.Word8"
    elif tp == AdvancedNumericTypes.INT_64:
        return "Data.Int.Int64"
    elif tp == AdvancedNumericTypes.INT_32:
        return "Data.Int.Int32"
    elif tp == AdvancedNumericTypes.INT_16:
        return "Data.Int.Int16"
    elif tp == AdvancedNumericTypes.INT_8:
        return "Data.Int.Int8"
    elif tp == AdvancedNumericTypes.SINGLE_PRECISION:
        return "Float"
    elif tp == AdvancedNumericTypes.DOUBLE_PRECISION:
        return "Double"
    elif tp == AdvancedNumericTypes.BIG_INT:
        return "Integer"
    elif tp == AdvancedStringTypes.CHAR:
        return "Char"
    basic = resolve_to_basic(tp)
    if basic == BasicBooleanTypes.BOOLEAN:
        return "Bool"
    elif basic == BasicStringTypes.TEXT:
        return "String"
    elif basic == BasicNumericTypes.INTEGER:
        return "Int"
    elif basic == BasicNumericTypes.REAL:
        return "Double"
    elif basic == BasicNothingTypes.NOTHING:
        return "Nothing"
    raise AssertionError(f"Unknown type: {tp!r}")


def convert_statement(statement: Statement, lifting=False) -> str:
    if isinstance(statement, Expression):
        result = ""
        if lifting:
            result += "return ("
        if isinstance(statement, Identifier):
            result += statement
        elif isinstance(statement, FunctionCall):
            result += convert_function_call(statement)
        else:
            assert isinstance(statement, Value)
            result += convert_value(statement)
        if lifting:
            result += ")"
        return result
    else:
        assert isinstance(statement, VariableAssignment)
        return f"let {statement.variable} = {convert_statement(statement.expression)}"


indent = " " * 4


def convert_execution_unit(pu: PreparedExecutionUnit) -> str:
    result = f"""{{-# LANGUAGE NamedFieldPuns #-}}
module {pu.unit.name} where

import System.IO (hPutStr, stderr, stdout, hFlush)
import System.Environment
import qualified Values
import Control.Monad.Trans.Class
import Control.Exception
import EvaluationUtils
import Data.Int
import Data.Word
"""

    for name in pu.evaluator_names:
        result += f"import qualified {name}\n"

    result += f"""
import qualified {pu.submission_name}

value_file = "{pu.value_file}"
exception_file = "{pu.exception_file}"

writeSeparator :: IO ()
writeSeparator = do
    hPutStr stderr "--{pu.testcase_separator_secret}-- SEP"
    hPutStr stdout "--{pu.testcase_separator_secret}-- SEP"
    appendFile value_file "--{pu.testcase_separator_secret}-- SEP"
    appendFile exception_file "--{pu.testcase_separator_secret}-- SEP"
    hFlush stdout
    hFlush stderr

writeContextSeparator :: IO ()
writeContextSeparator = do
    hPutStr stderr "--{pu.context_separator_secret}-- SEP"
    hPutStr stdout "--{pu.context_separator_secret}-- SEP"
    appendFile value_file "--{pu.context_separator_secret}-- SEP"
    appendFile exception_file "--{pu.context_separator_secret}-- SEP"
    hFlush stdout
    hFlush stderr

sendValue :: Values.JsonTypeable a => a -> IO ()
sendValue = Values.sendValue value_file

sendException :: Maybe SomeException -> IO ()
sendException = Values.sendException exception_file

sendSpecificValue :: EvaluationResult -> IO ()
sendSpecificValue = Values.sendEvaluated value_file

sendSpecificException :: EvaluationResult -> IO ()
sendSpecificException = Values.sendEvaluated exception_file

handleException :: Exception e => (Either e a) -> Maybe e
handleException (Left e) = Just e
handleException (Right _) = Nothing
"""

    # Generate code for each context.
    ctx: PreparedContext
    for i, ctx in enumerate(pu.contexts):
        result += f"{pu.unit.name.lower()}Context{i} :: IO ()\n"
        result += f"{pu.unit.name.lower()}Context{i} = do\n"
        result += indent + ctx.before + "\n"

        # Generate code for each testcase
        tc: PreparedTestcase
        for i1, tc in enumerate(ctx.testcases):
            result += indent + "writeSeparator\n"

            if tc.testcase.is_main_testcase():
                assert isinstance(tc.input, MainInput)
                wrapped = [json.dumps(a) for a in tc.input.arguments]
                result += indent + f"let mainArgs = [{' ,'.join(wrapped)}]\n"
                result += (
                    indent
                    + f"result <- try (withArgs mainArgs {pu.submission_name}.main) :: IO (Either SomeException ())\n"
                )
                result += (
                    indent
                    + f"let ee = handleException result in {convert_statement(tc.exception_statement('ee'))}\n"
                )
            else:
                assert isinstance(tc.input, PreparedTestcaseStatement)
                # In Haskell we do not have statements, so we need to keep them separate.
                # Additionally, exceptions with "statements" are not supported at this time.
                if is_statement_strict(tc.input.statement):
                    result += (
                        indent
                        + convert_statement(tc.input.unwrapped_input_statement())
                        + "\n"
                    )
                else:
                    result += indent + f"result{i1} <- catch\n"
                    result += indent * 2 + f"(do\n"  # Start a `do` block
                    id_result = tc.input.input_statement("r")
                    result += (
                        indent * 3
                        + f"r <- {convert_statement(tc.input.unwrapped_input_statement(), True)}\n"
                    )  # Bind the result to 'r'
                    if isinstance(id_result, Identifier):
                        result += (
                            indent * 3 + f"sendValue {convert_statement(id_result)}\n"
                        )  # Send the value if it's an identifier
                    else:
                        result += (
                            indent * 3 + convert_statement(id_result) + "\n"
                        )  # Otherwise, execute the statement
                    result += indent * 3 + "return ()\n"  # Explicitly return unit
                    result += indent * 2 + ") (\\e -> do\n"  # Handle exceptions
                    result += (
                        indent * 3
                        + f"let ee = (Just (e :: SomeException)) in {convert_statement(tc.exception_statement('ee'))}\n"
                    )
                    result += (
                        indent * 3 + "return ()\n"
                    )  # Explicitly return unit in the exception case
                    result += indent * 2 + ")\n"  # End the `catch` block
        result += indent + ctx.after + "\n"
        result += indent + 'putStr ""\n'

    result += """
main :: IO ()
main = do
"""
    for i, ctx in enumerate(pu.contexts):
        result += indent + "writeContextSeparator\n"
        result += indent + f"{pu.unit.name.lower()}Context{i}\n"

    result += indent + 'putStr ""\n'
    return result


def convert_selector(contexts: list[str]) -> str:
    result = """
module Selector where

import System.Environment
"""
    for ctx in contexts:
        result += f"import qualified {ctx}\n"

    result += """
main = do
    [n] <- getArgs
    case n of
"""

    for ctx in contexts:
        result += indent * 2 + f'"{ctx}" -> {ctx}.main\n'

    return result


def convert_check_function(evaluator: str, function: FunctionCall) -> str:
    return f"""
module EvaluatorExecutor where

import qualified {evaluator}
import Values
import System.IO (stdout)


main = do x <- return $ {convert_function_call(function)}
          sendEvaluatedH stdout x
"""


def convert_encoder(values: list[Value]) -> str:
    result = """
module Encode where

import Values
import System.IO (stdout)
import Data.Int
import Data.Word

main = do
"""

    for value in values:
        result += indent + f"sendValueH stdout ({convert_value(value)})\n"
        result += indent + 'putStr "␞"\n'
    return result
