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

        {# Open our file we use to write. -#}
        FileWriter {{ code_identifier }}_writer = new FileWriter("{{ output_file }}" );

        {# In Java, we must execute the before and after code in the context. #}
        {{ before or "" }}

        {# Call the main fucnction. #}
        {%- with function=execution %}
        {%- include "function.jinja2" -%};
        {%- endwith %}

        {% for additional in additionals -%}
        System.err.print("--{{ code_identifier }}-- SEP" );
        System.out.print("--{{ code_identifier }}-- SEP" );
        {{ code_identifier }}_writer.write("--{{ code_identifier }}-- SEP" );

        {%- with function=additional.input.function %}
        Values.send({{ code_identifier }}_writer, {{ name }}.{% include "function.jinja2" %});
        {%- endwith -%}
        {%- endfor %}

        {{ after or "" }}

        {{ code_identifier }}_writer.close();
    }
}
