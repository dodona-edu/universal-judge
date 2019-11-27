import sys
import values


def evaluated(result, expected=None, messages=[]):
    values.send_evaluated(sys.stdout, result, expected, None, messages)


${evaluator_code}

evaluate(\
<%include file="value.mako" args="value=expected" />
, \
<%include file="value.mako" args="value=actual" />
)
