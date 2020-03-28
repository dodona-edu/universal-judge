#include <string.h>

#define INCLUDED true

% for c in contexts:
    #include "${c}.c"
% endfor

int main(int argc, const char* argv[]) {

    const char* name = argv[1];

    % for c in contexts:
        if (strcmp("${c}", name) != 0) {
            ${c}();
        }
    % endfor
}
