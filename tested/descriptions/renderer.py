"""
A Marko renderer that only renders TESTed code; all other things are left alone.
"""

from marko import block
from marko.md_renderer import MarkdownRenderer

from tested.configs import Bundle
from tested.dsl import parse_dsl, parse_string
from tested.judge.evaluation import Channel, guess_expected_value, should_show
from tested.languages.generation import generate_statement, get_readable_input
from tested.testsuite import ExceptionOutputChannel, OutputChannel, Testcase


def render_one_statement(bundle: Bundle, statement: str) -> str:
    """
    Render a single statement.
    """
    parsed_string = parse_string(statement)
    generated_statement = generate_statement(bundle, parsed_string)
    # Allow the language to modify the template a bit.
    return bundle.language.cleanup_description(generated_statement)


# Similar to _add_channel
def _add_output(
    bundle: Bundle, output: OutputChannel, channel: Channel, results: list[str]
):
    if should_show(output, channel):
        expected = guess_expected_value(bundle, output)
        # Special handling of exceptions
        if channel == Channel.EXCEPTION:
            assert isinstance(output, ExceptionOutputChannel)
            if output.exception and not output.exception.get_type(
                bundle.config.programming_language
            ):
                name = bundle.language.get_declaration_metadata().get(
                    "exception", "Exception"
                )
                expected = f"{name}: {expected}"

        results.append(expected)


def get_expected_output(bundle: Bundle, tc: Testcase) -> list[str]:
    results = []
    _add_output(bundle, tc.output.stdout, Channel.STDOUT, results)
    _add_output(bundle, tc.output.stderr, Channel.STDERR, results)
    _add_output(bundle, tc.output.file, Channel.FILE, results)
    _add_output(bundle, tc.output.exception, Channel.EXCEPTION, results)
    _add_output(bundle, tc.output.result, Channel.RETURN, results)
    _add_output(bundle, tc.output.exit_code, Channel.EXIT, results)

    return results


class TestedRenderer(MarkdownRenderer):
    bundle: Bundle

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

    def _render_dsl_statements(self, element: block.FencedCode) -> str:
        """
        Render a single statement (or multiple lines of single statements).
        """
        assert element.lang == "dsl"

        rendered_dsl = self.render_children(element)

        # Parse the DSL
        parsed_dsl_with_messages = parse_dsl(rendered_dsl)

        # Get all actual tests
        tests = []
        for tab in parsed_dsl_with_messages.data.tabs:
            for context in tab.contexts:
                for testcase in context.testcases:
                    tests.append(testcase)

        resulting_lines = []
        prompt = self.bundle.language.get_declaration_metadata().get("prompt", ">")
        for testcase in tests:
            stmt_message, _ = get_readable_input(self.bundle, testcase)
            resulting_lines.append(f"{prompt} {stmt_message.description}")
            output_lines = get_expected_output(self.bundle, testcase)
            resulting_lines.extend(output_lines)

        language = (
            f"console?lang={self.bundle.config.programming_language}&prompt={prompt}"
        )
        body = "\n".join(resulting_lines)

        return f"```{language}\n{body}```\n"

    def render_fenced_code(self, element: block.FencedCode) -> str:
        if element.lang == "tested":
            return self._render_normal_statements(element)
        elif element.lang == "dsl":
            return self._render_dsl_statements(element)
        else:
            return super().render_fenced_code(element)
