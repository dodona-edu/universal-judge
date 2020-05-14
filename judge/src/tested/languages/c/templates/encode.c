#include <stdio.h>
#include <stdlib.h>

#include "values.h"

int main() {
    % for statement in statements:
        write_value(stdout, <%include file="statement.mako" args="statement=statement"/>);
        printf("\n");
    % endfor
}
