#include <string.h>

#define INCLUDED true

% for c in contexts:
    #include "${c}.c"
% endfor

int main(int argc, const char* argv[]) {

    const char* name = argv[1];

    if (name == NULL) {
        fprintf(stderr, "You must select the context");
        return -2;
    }

    % for c in contexts:
        if (strcmp("${c}", name) == 0) {
            return ${c}();
        }
    % endfor
    return -1;
}
