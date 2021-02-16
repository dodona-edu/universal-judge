import os
import re
import sys

from argparse import ArgumentParser, FileType
from functools import partial
from mako.template import Template

from tested.configs import Bundle, DodonaConfig
from tested.testplan import Plan
from tested.utils import smart_close
from tested.languages import get_language, language_exists


def _stmt_expr(m: re.Match) -> str:
    func = "statement" if m.group(1) == ">" else "expression"
    return f'${{{func}("""{m.group(2)}""")}}'


def _mako_uncomment(m: re.Match) -> str:
    result = f"${{{repr(m.group(1))}}}"
    return result


def create_description_instance(template: str,
                                programming_language: str = "python",
                                natural_language: str = "en",
                                namespace: str = "submission",
                                is_html: bool = True) -> str:
    if not language_exists(programming_language):
        raise ValueError(f"Language {programming_language} doesn't exists")

    regex_markdown = re.compile(r"^(```.*)\n\r?(((?!```).*\n\r?)*)(```)$",
                                re.MULTILINE)
    regex_comment_mako = re.compile(r"(?m)^(\s*#.*)$")
    regex_stmt_expr = re.compile(r"^([>=])\s*(.+(\n\r?(?![>=]).+)*)$", re.MULTILINE)

    last_end, mako_template = 0, []

    for match in regex_markdown.finditer(template):
        groups = match.groups()
        span = match.span(0)
        mako_template.extend(
            regex_comment_mako.sub(_mako_uncomment, template[last_end:span[0]]))
        last_end = span[1]
        mako_template.extend(groups[0])
        mako_template.append('\n')
        body = groups[1]
        body = regex_stmt_expr.sub(_stmt_expr, body)
        mako_template.extend(body)
        mako_template.extend(groups[3])

    mako_template.extend(
        regex_comment_mako.sub(_mako_uncomment, template[last_end:]))
    mako_template = ''.join(mako_template)

    template = Template(mako_template, cache_enabled=False)

    language = get_language(programming_language)

    bundle = Bundle(
        config=DodonaConfig(
            resources="",
            source="",
            time_limit=0,
            memory_limit=0,
            natural_language=natural_language,
            programming_language=programming_language,
            workdir="",
            judge=""
        ),
        out=open(os.devnull, 'w'),
        lang_config=language,
        context_separator_secret="",
        secret="",
        plan=Plan(namespace=namespace)
    )

    return template.render(
        appendix=partial(language.get_appendix, bundle=bundle, is_html=is_html),
        function_name=partial(language.get_function_name, is_html=is_html),
        type_name=partial(language.get_type_name, bundle=bundle, is_html=is_html),
        code_start=partial(language.get_code_start, is_html=is_html),
        code_end=partial(language.get_code_end, is_html=is_html),
        statement=partial(language.get_code, bundle=bundle, is_html=is_html,
                          statement=True),
        expression=partial(language.get_code, bundle=bundle, is_html=is_html,
                           statement=False),
        prompt=language.get_prompt(is_html=is_html),
        language=language.get_prompt_language(is_html=is_html)
    )


if __name__ == "__main__":
    parser = ArgumentParser(
        description="Translate description for language"
    )
    parser.add_argument('-d', '--description', type=FileType('r'),
                        help="Description template", default="-")
    parser.add_argument('-l', '--language', type=str, help="Programming language",
                        default="python")
    parser.add_argument('-o', '--output', type=FileType('w'),
                        help="Output description instance", default="-")
    parser.add_argument('-i', '--i18n', type=str,
                        help="Natural language (en, nl), default: en", default="en")
    parser.add_argument('-n', '--namespace', type=str,
                        help="Namespace of the submission", default="submission")

    type_group = parser.add_mutually_exclusive_group()
    type_group.add_argument('-M', '--markdown', action='store_true',
                            help="Generate markdown")
    type_group.add_argument('-H', '--html', action='store_true',
                            help="Generate html (default)")

    parser = parser.parse_args()
    with smart_close(parser.description) as template_fd:
        template_str = template_fd.read()

    try:
        rendered = create_description_instance(template_str, parser.language,
                                               parser.i18n, parser.namespace,
                                               not bool(parser.markdown))
    except ValueError as e:
        print(e, file=sys.stderr)
        sys.exit(-1)

    with smart_close(parser.output) as output_fd:
        print(rendered, file=output_fd)
