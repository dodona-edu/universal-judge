## Code to execute one test context.
<%! from testplan import Assignment %>
import sys
import evaluators

## Get which context we are currently testing.
number = int(sys.argv[1])

## Depending on the context, there may be some arguments
% for c in contexts:
    if ${loop.index} == number:
        import c.name
% endfor
