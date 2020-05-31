#include <string.h>
#include <stdio.h>

#define INCLUDED true

% for cont in contexts:
    #include "${cont}.c"
% endfor

int main(int argc, const char* argv[]) {

    if (argc < 1) {
        fprintf(stderr, "No context selected.");
        return -2;
    }
    
    const char* name = argv[1];
    % for cont in contexts:
        if (strcmp("${cont}", name) == 0) {
            return ${cont}();
        }
    % endfor
    fprintf(stderr, "Non-existing context '%s' selected.", name);
    return -1;
}
