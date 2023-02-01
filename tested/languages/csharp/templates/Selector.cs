<%page args="contexts" />
using System;

namespace Tested {
    class Selector {
        public static void Main(string[] args) {
            String name = args[0];
            % for c in contexts:
                if ("${c}" == name) {
                    ${c}.Main(Array.Empty<String>());
                }
            % endfor
        }
    }
}
