"""
Utilities for running code.
"""
import re

INDENT_START_TAGS = ["for", "while", "if", "else", "elif"]
INDENT_STOP_TAGS = ["endfor", "endwhile", "endif", "else", "elif"]


def is_control_start(line):
    return line.strip().startswith(tuple(f"% {x}" for x in INDENT_START_TAGS))


def is_control_end(line):
    return line.strip().startswith(tuple(f"% {x}" for x in INDENT_STOP_TAGS))


def strip(text, n):
    to_strip = min(n, count_indent(text))
    return text[to_strip:]


def count_indent(text):
    return len(text) - len(text.lstrip())


def is_tag(line):
    return line.endswith("%>") or re.compile("<%.*/>").match(line)


def remove_indents(code):
    """
    This preprocessor will remove indents from templates in mako.

    For example, consider following template:

    .. code-block:: mako
       % for additional in additionals:
           function ${context_id}_${i}_eval(value):
               % if isinstance(additional.output.result.evaluator, BuiltinEvaluator):
               ${code_identifier}_values.send(${code_identifier}_file, value)
               % endif
       % endfor

    Will result in:

    .. code-block:: mako
       % for additional in additionals:
       function ${context_id}_${i}_eval(value):
           % if isinstance(additional.output.result.evaluator, BuiltinEvaluator):
           ${code_identifier}_values.send(${code_identifier}_file, value)
           % endif
       % endfor
    """
    # We could use fancy regex, but just looping through the lines is probably enough.

    lines = code.splitlines()
    resulting_lines = []
    total_strip = 0
    last_added_strip = 0
    next_line_strip = False
    last_control_indent = 0

    print("Beginning handling.")
    for line in lines:
        print(f"Handling line: {line}")
        if is_control_end(line):
            print(f"-> is end of control, removing {last_added_strip}")
            total_strip -= last_added_strip
        elif next_line_strip:
            max_strip = min(4, count_indent(line) - last_control_indent)
            print(f"-> previous was control, adding {max_strip}")
            next_line_strip = False
            total_strip += max_strip
            last_added_strip = max_strip
        # Strip indent from line
        if is_control_start(line):
            print(f"-> is control start, next will strip to {count_indent(line)}")
            next_line_strip = True
            last_control_indent = count_indent(line)

        print(f"-> stripping max {total_strip} from line")
        line = strip(line, total_strip)
        resulting_lines.append(line)

    r = "\n".join(resulting_lines)
    print(r)
    return r


def remove_newline(code):
    """
    Remove newlines after tags, by appending slashes. For example,

    .. code-block: mako
       <%! from testplan import BuiltinEvaluator %>

    .. code-block: mako
       <%! from testplan import BuiltinEvaluator %>\
    """
    lines = code.splitlines()
    resulting_output = []

    for line in lines:
        if is_tag(line):
            resulting_output.append(f"{line}\\")
        else:
            resulting_output.append(line)

    r = "\n".join(resulting_output)
    print(r)
    return r
