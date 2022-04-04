## A test templates that receives a value and prints that value.
import sys
import values
from decimal import Decimal

% for statement in statements:
    values.send_value(sys.stdout, <%include file="statement.mako" args="statement=statement"/>)
    print()
% endfor

