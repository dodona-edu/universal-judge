#include <string.h>
#include <stdio.h>

#define INCLUDED true

% for context in contexts:
    #include "${context}.c"
% endfor

int main(int argc, const char* argv[]) {

    if (argc < 1) {
        fprintf(stderr, "No context selected.");
        return -2;
    }
    
    const char* name = argv[1];
    % for context in contexts:
        if (strcmp("${c}", name) == 0) {
            return ${c}();
        }
    % endfor
    fprintf(stderr, "Non-existing selector '%s' selected.", name);
    return -1;
}
