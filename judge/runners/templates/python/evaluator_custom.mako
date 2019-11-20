import sys
import values


def evaluated(result, messages=[]):
    values.send_evaluated(sys.stdout, result, None, None, messages)


${evaluator_code}

evaluate(\
<%include file="value.mako" args="value=expected" />
, \
<%include file="value.mako" args="value=actual" />
)
