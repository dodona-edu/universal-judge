import logging
from pathlib import Path
from typing import List, Tuple, Optional

from ..config import CallbackResult, Command, Config, Language
from ...configs import Bundle
from ...dodona import Message, Status, AnnotateCode
from ...internationalization import get_i18n_string

logger = logging.getLogger(__name__)

# Where the results of the compilation are stored.
OUTPUT_DIRECTORY = "all-outputs"


class CSharp(Language):
    def compilation(self, bundle: Bundle, files: List[str]) -> CallbackResult:
        # In C#, all output files are located in a subdirectory, so we just
        # want to copy over the subdirectory.
        def file_filter(file: Path) -> bool:
            return file.parent.name == OUTPUT_DIRECTORY

        executable_file = files[-1]
        name = Path(executable_file).stem
        args = [
            "dotnet",
            "build",
            "--no-incremental",
            "--output",
            OUTPUT_DIRECTORY,
            "--force",
            "--nologo",
            f"-p:AssemblyName={name}",
            f"-p:StartupObject=Tested.{name}",
        ]

        return args, file_filter

    def execution(
        self, config: Config, cwd: Path, file: str, arguments: List[str]
    ) -> Command:
        file = OUTPUT_DIRECTORY + "/" + file
        return ["dotnet", file, *arguments]

    def find_main_file(
        self, files: List[Path], name: str
    ) -> Tuple[Optional[Path], List[Message], Status, List[AnnotateCode]]:
        # TODO: specify the extension (if any) of the output files, so we don't need to
        # override this.
        logger.debug("Finding %s in %s", name, files)
        messages = []
        possible_main_files = [
            x for x in files if x.name.startswith(name) and x.suffix == ".dll"
        ]
        if possible_main_files:
            return possible_main_files[0], messages, Status.CORRECT, []
        else:
            messages.append(get_i18n_string("languages.config.unknown.compilation"))
            return None, messages, Status.COMPILATION_ERROR, []

    # noinspection PyTypeChecker
    def solution(self, solution: Path, bundle: Bundle):
        with open(solution, "r") as file:
            contents = file.read()

        if "class" in contents:
            return  # No top-level statements; we are happy...

        class_name = bundle.lang_config.submission_name(bundle.plan)
        result = f"""\
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
        {contents}
    }}
}}
        """

        with open(solution, "w") as file:
            file.write(result)
