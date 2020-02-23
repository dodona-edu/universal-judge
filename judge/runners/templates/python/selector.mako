## Code to execute_module one test context.
<%! from testplan import Assignment %>
import sys

## Get which context we are currently testing.
context_name = sys.argv[1]

## Depending on the context, there may be some arguments
## TODO: dynamically import the context?
% for c in contexts:
    if "${c}" == context_name:
        import ${c}
% endfor
