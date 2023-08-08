"""
A Marko renderer that only renders TESTed code; all other things are left alone.
"""
from marko import block
from marko.md_renderer import MarkdownRenderer

from tested.configs import Bundle
from tested.dsl import parse_string
from tested.languages.generation import generate_statement


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

    def render_fenced_code(self, element: block.FencedCode) -> str:
        if element.lang not in ("tested", "console?lang=tested"):
            return super().render_fenced_code(element)

        rendered_lines = self.render_children(element).splitlines()
        resulting_lines = []
        one_console = False
        for rendered_line in rendered_lines:
            console = False
            # We have a console line, so strip the prefix before processing it.
            if rendered_line.startswith(">>>"):
                one_console = True
                rendered_line = rendered_line[3:]
                rendered_line = rendered_line.lstrip()
                console = True

            generated_statement = render_one_statement(self.bundle, rendered_line)
            # Re-add the correct console prefix if this was a console line.
            if console:
                prefix = (
                    self.bundle.lang_config.get_declaration_metadata().get(
                        "prompt", ">"
                    )
                    + " "
                )
            else:
                prefix = ""
            resulting_lines.append(prefix + generated_statement)
        if one_console:
            prompt = self.bundle.lang_config.get_declaration_metadata().get(
                "prompt", ">"
            )
            language = f"console?lang={self.bundle.config.programming_language}&prompt={prompt}"
        else:
            language = self.bundle.config.programming_language
        return "```" + language + "\n" + "\n".join(resulting_lines) + "\n```\n"
