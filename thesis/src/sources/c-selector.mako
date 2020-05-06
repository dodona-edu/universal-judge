#include <string.h>
#include <stdio.h>

#define INCLUDED true

% for c in contexts:
    #include "${c}.c"
% endfor

int main(int argc, const char* argv[]) {

    if (argc < 1) {
        fprintf(stderr, "You must select the context");
        return -2;
    }
    
    const char* name = argv[1];

    % for c in contexts:
        if (strcmp("${c}", name) == 0) {
            return ${c}();
        }
    % endfor
    return -1;
}
