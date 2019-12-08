import sys
import values


def evaluated(result, expected=None, actual=None, messages=[]):
    values.send_evaluated(sys.stdout, result, expected, actual, messages)


${evaluator_code}

evaluate(\
<%include file="value.mako" args="value=expected" />
, \
<%include file="value.mako" args="value=actual" />
)
