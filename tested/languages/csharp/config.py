import logging
import re
import subprocess
from collections.abc import Iterable
from functools import cache
from pathlib import Path
from typing import TYPE_CHECKING

from tested.datatypes import AllTypes
from tested.dodona import AnnotateCode, Message, Status
from tested.features import Construct, TypeSupport
from tested.languages.conventionalize import (
    EXECUTION_PREFIX,
    Conventionable,
    NamingConventions,
    conventionalize_namespace,
    submission_file,
    submission_name,
)
from tested.languages.language import (
    CallbackResult,
    Command,
    Language,
    TypeDeclarationMetadata,
)
from tested.serialisation import Statement, Value

logger = logging.getLogger(__name__)

if TYPE_CHECKING:
    from tested.languages.generation import PreparedExecutionUnit

# The symbols MSBuild defines for a Debug build of net8.0.
DEFINES = [
    "TRACE",
    "DEBUG",
    "NET",
    "NET8_0",
    "NETCOREAPP",
    "NET5_0_OR_GREATER",
    "NET6_0_OR_GREATER",
    "NET7_0_OR_GREATER",
    "NET8_0_OR_GREATER",
    "NETCOREAPP1_0_OR_GREATER",
    "NETCOREAPP1_1_OR_GREATER",
    "NETCOREAPP2_0_OR_GREATER",
    "NETCOREAPP2_1_OR_GREATER",
    "NETCOREAPP2_2_OR_GREATER",
    "NETCOREAPP3_0_OR_GREATER",
    "NETCOREAPP3_1_OR_GREATER",
]


def _newest(paths: Iterable[Path], version_part: int, what: str) -> Path:
    def version(path: Path) -> tuple[int, ...]:
        return tuple(int(x) for x in re.findall(r"\d+", path.parts[version_part]))

    paths = list(paths)
    if not paths:
        raise FileNotFoundError(f"No {what} found in the .NET installation")
    return max(paths, key=version)


@cache
def _compiler_and_references() -> tuple[Path, list[Path]]:
    """
    Find the C# compiler and the .NET 8 reference assemblies in the SDK.

    TESTed calls the compiler directly: `dotnet build` spends seconds on MSBuild
    for a project that never changes, and leaves a compiler server running that
    competes with the executions for the CPU.
    """
    # dotnet can be a launcher script (as with Homebrew), so ask it where the
    # SDKs are. Each line looks like "8.0.405 [/usr/share/dotnet/sdk]".
    listing = subprocess.run(
        ["dotnet", "--list-sdks"], capture_output=True, text=True, check=True
    ).stdout
    sdks = [
        Path(directory) / version
        for version, directory in re.findall(r"^(\S+) \[(.+)\]$", listing, re.M)
    ]
    sdk = _newest(sdks, -1, ".NET SDK")
    roots = {x.parent.parent for x in sdks}
    packs = [
        reference
        for root in roots
        for reference in root.glob("packs/Microsoft.NETCore.App.Ref/*/ref/net8.0")
    ]
    references = _newest(packs, -3, "net8.0 reference assemblies")
    return sdk / "Roslyn" / "bincore" / "csc.dll", sorted(references.glob("*.dll"))


class CSharp(Language):
    def initial_dependencies(self) -> list[str]:
        return ["GlobalUsings.cs", "Values.cs", "EvaluationResult.cs"]

    def needs_selector(self):
        return True

    def file_extension(self) -> str:
        return "cs"

    def comment(self, text: str) -> str:
        return f"// {text}"

    def naming_conventions(self) -> dict[Conventionable, NamingConventions]:
        return {
            "namespace": "pascal_case",
            "function": "pascal_case",
            "identifier": "pascal_case",
            "property": "pascal_case",
            "class": "pascal_case",
            "global_identifier": "macro_case",
        }

    def supported_constructs(self) -> set[Construct]:
        return {
            Construct.OBJECTS,
            Construct.EXCEPTIONS,
            Construct.FUNCTION_CALLS,
            Construct.ASSIGNMENTS,
            Construct.HETEROGENEOUS_COLLECTIONS,
            Construct.HETEROGENEOUS_ARGUMENTS,
            Construct.EVALUATION,
            Construct.NAMED_ARGUMENTS,
            Construct.DEFAULT_PARAMETERS,
            Construct.GLOBAL_VARIABLES,
        }

    def datatype_support(self) -> dict[AllTypes, TypeSupport]:
        return {  # type: ignore
            "integer": "supported",
            "real": "supported",
            "char": "reduced",
            "string": "supported",
            "text": "supported",
            "boolean": "supported",
            "sequence": "supported",
            "set": "supported",
            "map": "supported",
            "dictionary": "supported",
            "object": "reduced",
            "nothing": "supported",
            "undefined": "reduced",
            "null": "reduced",
            "int8": "supported",
            "uint8": "supported",
            "int16": "supported",
            "uint16": "supported",
            "int32": "supported",
            "uint32": "supported",
            "int64": "supported",
            "uint64": "supported",
            "bigint": "supported",
            "single_precision": "supported",
            "double_precision": "supported",
            "array": "supported",
            "list": "supported",
            "tuple": "supported",
        }

    def compilation(self, files: list[str]) -> CallbackResult:
        name = Path(files[-1]).stem

        # Only the compiler's own output: the directory also holds the workdir's
        # subdirectories, which may contain other .dll files.
        def file_filter(file: Path) -> bool:
            return file.name in (f"{name}.dll", f"{name}.pdb")

        compiler, references = _compiler_and_references()
        args = [
            "dotnet",
            str(compiler),
            "-nologo",
            "-noconfig",
            "-nostdlib",
            "-fullpaths",
            "-utf8output",
            "-target:exe",
            f"-main:Tested.{name}",
            f"-out:{name}.dll",
            "-debug:portable",
            "-optimize-",
            "-nullable:enable",
            "-langversion:12.0",
            "-warn:8",
            "-nowarn:1701,1702",
            f"-define:{';'.join(DEFINES)}",
            *(f"-r:{x}" for x in references),
            # Like `dotnet build`, compile every source file in the directory,
            # including those in subdirectories copied from the workdir.
            "-recurse:*.cs",
        ]

        return args, file_filter

    def execution(self, cwd: Path, file: str, arguments: list[str]) -> Command:
        # csc does not write the runtimeconfig.json that `dotnet build` does.
        runtime_config = self.path_to_dependencies()[0] / "tested.runtimeconfig.json"
        return [
            "dotnet",
            "exec",
            "--runtimeconfig",
            str(runtime_config),
            file,
            *arguments,
        ]

    def find_main_file(self, files: list[Path], name: str) -> Path | Status:
        # TODO: specify the extension (if any) of the output files, so we don't need to
        # override this.
        logger.debug("Finding %s in %s", name, files)
        possible_main_files = [
            x for x in files if x.name.startswith(name) and x.suffix == ".dll"
        ]
        if possible_main_files:
            return possible_main_files[0]
        else:
            return Status.COMPILATION_ERROR

    def modify_solution(self, solution: Path):
        assert self.config
        with open(solution, "r") as file:
            contents = file.read()

        if "class" in contents:
            return  # No top-level statements; we are happy...

        class_name = submission_name(self)
        header = f"""\
using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http;
using System.Threading;
using System.Threading.Tasks;

class {class_name}
{{
    public static void Main(string[] args)
    {{
"""
        footer = """
    }
}
"""

        # noinspection PyTypeChecker
        with open(solution, "w") as file:
            file.write(header + contents + footer)

        self.config.dodona.source_offset -= header.count("\n")

    def cleanup_stacktrace(self, stacktrace: str) -> str:
        assert self.config
        execution = conventionalize_namespace(self, EXECUTION_PREFIX)
        execution_submission_location_regex = (
            rf"{self.config.dodona.workdir}/common/{execution}[0-9]+.cs"
        )
        workdir = re.escape(str(self.config.dodona.workdir.resolve()))
        subdir = rf"(?:common|{execution}[_0-9]+)"
        sub = re.escape(submission_file(self))
        submission_location_regex = rf"{workdir}/{subdir}/{sub}:line ([0-9]+)"
        compilation_location_regex = rf"{workdir}/{subdir}/{sub}\((\d+),(\d+)\)"

        resulting_lines = ""
        for line in stacktrace.splitlines(keepends=True):
            # If we encounter an execution location, we are done.
            if re.search(execution_submission_location_regex, line):
                break

            line = re.sub(submission_location_regex, r"<code>:\1", line)
            line = re.sub(compilation_location_regex, r"<code>:\1:\2", line)
            resulting_lines += line

        return resulting_lines

    def compiler_output(
        self, stdout: str, stderr: str
    ) -> tuple[list[Message], list[AnnotateCode], str, str]:
        submission = submission_name(self)
        message_regex = (
            rf"{submission}\((\d+),(\d+)\): (error|warning) ([A-Z0-9]+): (.*)"
        )
        messages = re.findall(message_regex, stdout)
        annotations = []
        for message in messages:
            annotations.append(
                AnnotateCode(
                    row=int(message[0]),
                    text=message[4],
                    externalUrl="https://learn.microsoft.com/dotnet/csharp/language-reference/compiler-messages/",
                    column=int(message[1]),
                    type=message[2],
                )
            )

        return [], annotations, stdout, stderr

    def generate_statement(self, statement: Statement) -> str:
        from tested.languages.csharp import generators

        return generators.convert_statement(statement, full=True)

    def generate_execution_unit(self, execution_unit: "PreparedExecutionUnit") -> str:
        from tested.languages.csharp import generators

        return generators.convert_execution_unit(execution_unit)

    def generate_selector(self, contexts: list[str]) -> str:
        from tested.languages.csharp import generators

        return generators.convert_selector(contexts)

    def generate_encoder(self, values: list[Value]) -> str:
        from tested.languages.csharp import generators

        return generators.convert_encoder(values)

    def get_declaration_metadata(self) -> TypeDeclarationMetadata:
        return {
            "names": {  # type: ignore
                "integer": "Int32",
                "real": "Double",
                "char": "char",
                "text": "string",
                "string": "string",
                "boolean": "Boolean",
                "sequence": "List",
                "set": "Set",
                "map": "Dictionary",
                "int8": "SByte",
                "uint8": "Byte",
                "int16": "Int16",
                "uint16": "UInt16",
                "int32": "Int32",
                "uint32": "UInt32",
                "int64": "Int64",
                "uint64": "UInt64",
                "bigint": "BigInteger",
                "single_precision": "Single",
                "double_precision": "Double",
                "double_extended": "BigInteger",
                "list": "List",
                "tuple": "Tuple",
                "any": "Object",
            },
            "exception": "Exception",
        }

    def is_void_method(self, name: str) -> bool:
        assert self.config
        regex = rf"void\s+{re.escape(name)}\b"
        the_source = self.config.dodona.source.read_text()
        return re.search(regex, the_source) is not None
