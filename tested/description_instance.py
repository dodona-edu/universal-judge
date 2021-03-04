import html
import os
import re
import sys

from argparse import ArgumentParser, FileType
from functools import partial
from typing import List

from mako.template import Template

from tested.configs import Bundle, DodonaConfig
from tested.languages.description_generator import TYPE_ARG, TYPE_CONFIG_NAME
from tested.testplan import Plan
from tested.utils import smart_close
from tested.languages import get_language, language_exists

open_brackets = ('(', '[', '{')
close_brackets = {')': '(', ']': '[', '}': '{'}


def _mako_uncomment(m: re.Match) -> str:
    result = f"${{{repr(m.group(1))}}}"
    return result


def _analyse_body(body: str) -> str:
    expressions, same, html_tag, stack = [], False, False, []
    for line in body.splitlines(keepends=False):
        line = line.lstrip()
        new_same = line and line[-1] == '\\'
        line = line[:-1] if new_same else line
        if same or stack:
            expressions[-1] += ' ' + line
        else:
            expressions.append(line)
        stack = _analyse_line(line, stack)
        same = new_same

    if stack:
        raise ValueError("Statement or expression brackets are not balanced")

    for index, expr in enumerate(expressions):
        if not expr:
            continue
        elif expr[0] == '>':
            expressions[index] = f"${{statement({repr(expr[1:].strip())})}}"
        else:
            expressions[index] = f"${{expression({repr(expr.strip())})}}"

    return "\n".join(expressions) + ("" if same else "\n")


def _analyse_line(line: str, stack: List[str]) -> List[str]:
    is_string, escape = False, False
    for c in line:
        if is_string:
            if c == '\\':
                escape = True
            elif c == '"':
                if not escape:
                    is_string = False
            else:
                escape = False
        elif c == '"':
            is_string = True
        elif c in open_brackets:
            stack.append(c)
        elif c in close_brackets:
            try:
                if close_brackets[c] != stack[-1]:
                    raise ValueError(
                        "Statement or expression brackets are not balanced")
                else:
                    stack.pop()
            except IndexError:
                raise ValueError(
                    "Statement or expression brackets are not balanced")
        else:
            continue
    if is_string:
        raise ValueError("String is not closed on this line")
    return stack


def prepare_template(template: str, is_html: bool = True) -> Template:
    if is_html:
        re_tag = "(\"[^\"]*\"|'[^']*'|[^'\">])*"

        regex_code, body_index = re.compile(
            fr"((<code{re_tag}) (class=\"([^\"]* )*tested( [^\"]*)*\")("
            fr"{re_tag}>)(\s*\\?))((?!</?code{re_tag}>).*?)(</code>)",
            re.MULTILINE | re.DOTALL
        ), -3
    else:
        regex_code, body_index = re.compile(
            r"^(```tested\r?\n)(((?!```).*\r?\n)*)(```)$", re.MULTILINE), 1

    regex_comment_mako = re.compile(r"(?m)^(\s*#.*)$")

    last_end, mako_template = 0, []

    for match in regex_code.finditer(template):
        groups = match.groups()
        span = match.span(0)
        mako_template.extend(
            regex_comment_mako.sub(_mako_uncomment, template[last_end:span[0]]))
        last_end = span[1]
        if is_html:
            mako_template.extend(groups[0])
        else:
            mako_template.extend("```console?lang=${language}&prompt=${prompt}\n")
        mako_template.extend(_analyse_body(groups[body_index]))
        mako_template.extend(groups[-1])

    mako_template.extend(
        regex_comment_mako.sub(_mako_uncomment, template[last_end:]))
    return Template(''.join(mako_template), cache_enabled=False)


def create_description_instance_from_template(template: Template,
                                              programming_language: str = "python",
                                              natural_language: str = "en",
                                              namespace: str = "submission",
                                              is_html: bool = True) -> str:
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

    description_generator = language.get_description_generator()

    # Partial function doesn't work because of bundle must be given,
    # but custom_type_map not
    def get_type_name(args: TYPE_ARG,
                      custom_type_map: TYPE_CONFIG_NAME = None) -> str:
        return description_generator.get_type_name(args, bundle, custom_type_map,
                                                   is_html=is_html)

    return template.render(
        function_name=partial(description_generator.get_function_name,
                              is_html=is_html),
        natural_type_name=partial(description_generator.get_natural_type_name,
                                  bundle=bundle, is_html=is_html),
        type_name=get_type_name,
        statement=partial(description_generator.get_code, bundle=bundle,
                          is_html=is_html, statement=True),
        expression=partial(description_generator.get_code, bundle=bundle,
                           is_html=is_html, statement=False),
        prompt=description_generator.get_prompt(is_html=is_html),
        language_html=description_generator.get_prompt_language(is_html=True),
        language=description_generator.get_prompt_language(is_html=False),
        namespace_html=html.escape(language.conventionalize_namespace(namespace)),
        namespace=language.conventionalize_namespace(namespace)
    )


def create_description_instance(template: str,
                                programming_language: str = "python",
                                natural_language: str = "en",
                                namespace: str = "submission",
                                is_html: bool = True) -> str:
    if not language_exists(programming_language):
        raise ValueError(f"Language {programming_language} doesn't exists")

    template = prepare_template(template, is_html)
    return create_description_instance_from_template(template, programming_language,
                                                     natural_language, namespace,
                                                     is_html)


if __name__ == "__main__":
    parser = ArgumentParser(
        description="Translate description for language",
        usage='%(prog)s [-h] [-d TEMPLATE] [-o INSTANCE] [-l PROGRAMMING_LANGUAGE] '
              '[-i NATURAL_LANGUAGE] [-n NAMESPACE] [(-M | -H)] '
              '[template [instance]]'
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

    parser.add_argument('template', metavar='template', nargs='?',
                        type=FileType('r'),
                        help="Where the template should be read from, override "
                             "option when given.")
    parser.add_argument('instance', metavar='instance', nargs='?',
                        type=FileType('w'),
                        help="Where the translate instance should be written to, "
                             "override option when given.")

    parser = parser.parse_args()
    if parser.template is not None:
        smart_close(parser.description)
        parser.description = parser.template
        if parser.instance is not None:
            smart_close(parser.output)
            parser.output = parser.instance

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
