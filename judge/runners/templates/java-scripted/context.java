{#
This file represents all code that will be executed for one context.
#}
import java.io.FileWriter;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

class Context{{ context_id }} {

    public static void main(String[] a) throws Exception {
        {# In scripted Java, we just execute the code, nothing else. #}
        {{ before or "" }}
        {{ code }}
        {{ after or "" }}
    }
}
