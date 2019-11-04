"""Configuration for languages, making implementing runners fairly easy."""
from typing import List, Optional

from testplan import Plan, Context


class LanguageConfig:
    """
    Configuration for the runner
    """

    # TODO: proper support for language features.
    def supports_top_level_functions(self) -> bool:
        """If the language supports top level functions."""
        raise NotImplementedError

    # TODO: proper support for language features.
    def needs_main(self):
        """If the language needs a main function."""
        raise NotImplementedError

    def needs_compilation(self) -> bool:
        """If the language needs compilation."""
        raise NotImplementedError

    def compilation_command(self, files: List[str]) -> List[str]:
        """Compile some files."""
        if self.needs_compilation():
            raise NotImplementedError
        else:
            return []

    def execution_command(self, context_id: str) -> List[str]:
        raise NotImplementedError

    def file_extension(self) -> str:
        """The file extension for this language, without dot."""
        raise NotImplementedError

    def submission_name(self, context_id: str, context: Context) -> Optional[str]:
        """
        Produce a name for the submission file.

        :param context_id: The ID that has been assigned to this context, to be able to uniquely
                           define this context. Note that the submission file must be identifiable
                           with only the context ID.
        :param context: The actual context. This might be useful if you want to wrap the submission
                        code in a class, to name one example.
        :return: The name of the file. Must be a valid filename without extension, and also satisfy
                 language specific limitations on file names. For example, in Java, the name of the
                 file must match the name of the class. Return None if this language does not need
                 a separate submission file.
        """
        raise NotImplementedError

    def context_name(self, context_id: str) -> str:
        """
        Produce a name for the context file.

        :param context_id: he ID that has been assigned to this context, to be able to uniquely
                           define this context. Note that the context file must be identifiable
                           with only the context ID.
        :return: The name of the file. Must be a valid filename without extension, and also satisfy
                 language specific limitations on file names. For example, in Java, the name of the
                 file must match the name of the class.
        """
        raise NotImplementedError

    def additional_files(self) -> List[str]:
        """Additional files that will be available to the context tests."""
        raise NotImplementedError
