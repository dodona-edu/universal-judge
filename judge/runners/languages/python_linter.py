"""
Support linting Python code.
Most of this code is taken from the code from Pythia.
"""
import logging
from os import PathLike
from pathlib import Path
from typing import List, Tuple
from io import StringIO

from pylint import lint
from pylint.reporters import JSONReporter

from dodona import *

logger = logging.getLogger(__name__)

# localize judge output
translations = {
    'en': {
        'linting tab':     'code',
        'TLE description': 'time limit exceeded',
    },
    'nl': {
        'linting tab':     'code',
        'TLE description': 'tijdslimiet overschreden',
    },
}

# pylint configuration
# @formatter:off
pylint_settings = """
[BASIC]

function-rgx=[_A-Za-z0-9]{1,30}$
method-rgx=[_A-Za-z0-9]{1,30}$
attr-rgx=[_A-Za-z0-9_]{1,30}$
argument-rgx=[_A-Za-z0-9]{1,30}$
variable-rgx=[_A-Za-z0-9]{1,30}$
inlinevar-rgx=[_A-Za-z0-9]{1,30}$
const-rgx=[_A-Za-z0-9]{1,30}$

[MESSAGES CONTROL]

#-W0123 Eval is evil.
#-W0141 Used builtin function %r (input,zip,filter,...).
# W0142 Used * or * magic* Used when a function or method is called using *args or **kwargs to dispatch arguments.
#-W0201 Attribute %r defined outside __init__
#-W0212 Access to a protected member %s of a client class
#-W0232 Class has no __init__ method; used when a class has no __init__ method, neither its parent classes.
#-W0614 Unused import XYZ from wildcard import
# W0621 Redefining name %r from outer scope
# W0622 Redefining built-in %r
#-W0631 Using possibly undefined loop variable
#-W0702 No exception's type specified; used when an except clause doesn't specify exceptions type to catch.
#-W0704 Except doesn't do anything; used when an except clause does nothing but "pass" and there is no "else" clause
#-W1304 Unused format argument
#-R0201 Used when there is no reference to the class, suggesting that the method could be used as a static function instead
#-R0801 Similar lines in %s files
# R0902 Too many instance attributes (maximal 7 allowed)
# R0903 Too few public methods
#-R0904 Too many public methods
#-R0911 Too many return statements
#-R0912 Too many branches
#-R0913 Too many arguments
#-R0914 Too many local variables
#-R0915 Too many statements
# C0111 Missing docstring
# C0301 Line too long
# C0303 Trailing whitespace
# C0304 Final newline missing
# C0330 Wrong continued indentation.
# C0413 Import "{}" should be placed at the top of the module.
#-C1001 Old-style class defined
# I0011 Warning locally suppressed using disable-msg
# I0012 Warning locally suppressed using disable-msg
# old version: disable=I0011,I0012,W0704,W0142,W0212,W0232,W0702,R0201,W0614,R0914,R0912,R0915,R0913,R0904,R0801,C0303,C0111,C0304,R0903,W0141,W0621,C0301,W0631,R0911,C1001
disable=W0142,W0621,W0622,R0902,R0903,C0111,C0301,C0303,C0304,C0330,C0413,I0011,I0012
evaluation=max(10.0 - ((float(5 * error + warning + refactor + convention) / statement) * 10), 0)
"""
# @formatter:on

def run_pylint(path: Path, submission: Path) \
        -> Tuple[List[Message], List[AnnotateCode]]:
    """
    Calls pylint to annotate submitted source code and adds resulting score and
    annotations to tab.
    """

    # define categories used for pylint messages
    message_categories = {
        'fatal':      Severity.ERROR,
        'error':      Severity.ERROR,
        'warning':    Severity.WARNING,
        'convention': Severity.INFO,
        'refactor':   Severity.INFO
    }

    # TODO: this file could be included as a resource
    # TODO: check to see if the configuration needs to be passed as a file on the
    #  file system
    # write pylint configurations into working directory
    pylint_config: Union[Path, PathLike] = path / 'pylint_config.rc'
    fl = open(pylint_config, 'w', encoding='utf-8')
    print(pylint_settings, file=fl)
    fl.close()

    # run pylint as part of the current process
    args = [
        '--rcfile={}'.format(pylint_config),
        submission
    ]

    pylint_out = StringIO()
    try:
        logger.debug("Running with args %s", args)
        lint.Run(args, reporter=JSONReporter(output=pylint_out), do_exit=False)
    except Exception as e:
        logger.warning("Pylint crashed with", exc_info=e)
        return [
                   "pylint crashed",
                   ExtendedMessage(
                       description=str(e),
                       format='code',
                       permission=Permission.STAFF
                   )
               ], []

    # Handle output
    try:
        messages = json.loads(pylint_out.getvalue())
    except Exception as e:
        logger.warning("Pylint produced bad output", exc_info=e)
        return [
                   "Pylint produced bad output.",
                   ExtendedMessage(
                       description=str(e),
                       format='code',
                       permission=Permission.STAFF
                   )
               ], []

    annotations = []

    for message in messages:
        category = message_categories.get(message["type"], Severity.WARNING)
        logger.debug("Handling message %s", str(message))
        annotations.append(AnnotateCode(
            row=max(int(message["line"]) - 1, 0),
            column=max(int(message["column"]) - 1, 0),
            text=f"{message['message']} ({message['message-id']})",
            type=category
        ))

    # sort linting messages on line, column and code
    annotations.sort(key=lambda a: (a.row, a.column, a.text))
    # for now, reports are not processed
    return [], annotations


def run_linter(path: Path, submission: Union[Path, PathLike]) \
        -> Tuple[List[Message], List[AnnotateCode]]:
    logger.debug("Running py_lint...")
    # run pylint to collect evaluation score and annotations
    return run_pylint(path, submission)
