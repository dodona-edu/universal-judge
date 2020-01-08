"""
Small module with useful functions and classes for managing dependencies for an execution.

These classes and methods are used to ensure the correct files and dependencies end up in the final
folder where the submission will be tested.

Each dependency will be copied to the folder of the relevant step (pre_execution and execution).
Depending on the flags that are set, they will also be provided to the various callbacks. The
"lifecycle" is as follows:

1. All dependencies are copied to a "common" folder.
2. Relevant files are used to "precompile". This is the place to compile common code, such as the
   user submission. The precompilation callback returns a new list of dependencies. All dependencies
   that were passed to the function are removed from the set of dependencies, and the returned ones
   are inserted.
   If a language does not need precompilation, the existing files may be returned.
3. All dependencies are copied to the context folder.
4. Relevant files are passed to the compilation step. The compilation callback returns a new list of
   dependencies. All dependencies that were passed to the function are removed from the set of
   dependencies, while the newly returned ones are added.
   If a language does not need compilation, the existing files may be returned.
5. Relevant files are passed to the execution callback. Here, the tests are executed on to test the
   submission.

The whole process can be described as mapping functions on a list of files:

[dependencies] -> map(dependencies, precompile) -> [dependencies] -> ...

"""
import shutil
from dataclasses import dataclass, field
from enum import Flag, auto
from pathlib import Path

from typing import NamedTuple, List


class Destination(Flag):
    """
    Possible intended usages for the file.
    """
    PRE_COMPILATION = auto()
    COMPILATION = auto()
    EXECUTION = auto()


class File(NamedTuple):
    """A file that will need to processed when executing a judgement."""
    name: str
    destination: Destination

    @classmethod
    def create_submission(cls, name: str) -> 'File':
        return File(name, Destination.PRE_COMPILATION)


@dataclass
class Dependencies:
    """A collection of files."""
    files: List[File] = field(default_factory=list)

    def get_and_remove(self, destination: Destination) -> List[str]:
        """
        Get and remove all files with a given destination. If a file has other flags besides the
        requested one, it is still returned and removed.
        """
        filtered = []
        other = []
        for existing in self.files:
            # If the file has other flags, it is both retained and returned.
            if destination in existing.destination:
                filtered.append(existing.name)
            else:
                other.append(existing)
        self.files = other
        return filtered

    def get(self, destination: Destination) -> List[str]:
        """Get and remove all files with a given destination."""
        return [f.name for f in self.files if destination in f.destination]

    def add(self, files: List[File]):
        self.files.extend(files)

    def _find_file_path(self, paths: List[Path], base: str) -> Path:
        looked = []
        for potential in paths:
            if (p := potential / base).exists():
                return p
            looked.append(str(potential))
        raise FileNotFoundError(f"Did not find {base}, considered {looked}")

    def copy_to(self, bases: List[Path], destination: Path):
        for file in self.files:
            result = self._find_file_path(bases, file.name)
            # noinspection PyTypeChecker
            shutil.copy2(result, destination)
