"""
A Marko renderer that only renders TESTed code; all other things are left alone.
"""
from doctest import DocTestParser

from marko import block
from marko.md_renderer import MarkdownRenderer

from tested.configs import Bundle
from tested.dsl import parse_string
from tested.languages.generation import generate_statement

TESTED_EXAMPLE_FORMAT = "console?lang=tested"


def render_one_statement(bundle: Bundle, statement: str) -> str:
    """
    Render a single statement.
    """
    parsed_string = parse_string(statement)
    generated_statement = generate_statement(bundle, parsed_string)
    # Allow the language to modify the template a bit.
    return bundle.lang_config.cleanup_description(generated_statement)


class TestedRenderer(MarkdownRenderer):
    bundle: Bundle
    _doctest_parser: DocTestParser

    def __init__(self):
        super().__init__()
        self._doctest_parser = DocTestParser()

    def _render_doctest(self, element: block.FencedCode) -> str:
        """
        Render a "doctest" code block.
        """
        assert element.lang == TESTED_EXAMPLE_FORMAT
        raw_code = self.render_children(element)
        doctests = self._doctest_parser.get_examples(raw_code)

        resulting_lines = []
        prompt = self.bundle.lang_config.get_declaration_metadata().get("prompt", ">")

        # Both the doctests and the results are parsed as values in the DSL.
        for examples in doctests:
            generated_statement = render_one_statement(self.bundle, examples.source)
            resulting_lines.append(f"{prompt} {generated_statement}")
            resulting_lines.append(examples.want.lstrip())

        language = (
            f"console?lang={self.bundle.config.programming_language}&prompt={prompt}"
        )
        body = "\n".join(resulting_lines)

        return f"```{language}\n{body}```\n"

    def _render_normal_statements(self, element: block.FencedCode) -> str:
        """
        Render a single statement (or multiple lines of single statements).
        """
        assert element.lang == "tested"

        rendered_lines = self.render_children(element).splitlines()
        resulting_lines = []

        for rendered_line in rendered_lines:
            generated_statement = render_one_statement(self.bundle, rendered_line)
            resulting_lines.append(generated_statement)

        language = self.bundle.config.programming_language
        body = "\n".join(resulting_lines)
        return f"```{language}\n{body}\n```\n"

    def render_fenced_code(self, element: block.FencedCode) -> str:
        if element.lang == "tested":
            return self._render_normal_statements(element)
        elif element.lang == TESTED_EXAMPLE_FORMAT:
            return self._render_doctest(element)
        else:
            return super().render_fenced_code(element)
