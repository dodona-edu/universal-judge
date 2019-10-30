{#
This file represents all code that will be executed for one text context.
#}
import java.io.FileWriter;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

class Context{{ context_id }} {

    public static void main(String[] a) throws Exception {

        {# Open our file we use to write. #}
{#        File {{ code_identifier }}_file = new File("{{ output_file }}");#}
{#        {{ code_identifier }}_file.mkdirs();#}
{#        {{ code_identifier }}_file.createNewFile();#}
        FileWriter {{ code_identifier }}_writer = new FileWriter("{{ output_file }}");

        {# Set the arguments #}
        {%- if execution.arguments %}
        String[] args = new String[]{
                {%- for argument in execution.args -%}
                {%- include "argument.jinja2" -%}{{ ", " if not loop.last else "" }}
                {%- endfor -%}
        };
        {% else %}
        String[] args = new String[]{};
        {% endif %}

        {# Call the main fucnction. #}
        {#  #}
        {{ execution.object }}.main(args);

        {% for additional in additionals -%}
        System.err.print("--{{ code_identifier }}-- SEP");
        System.out.print("--{{ code_identifier }}-- SEP");
        {{ code_identifier }}_writer.write("--{{ code_identifier }}-- SEP");

        {%- with function=additional.input.function %}
        Values.send({{ code_identifier }}_writer, {% include "function.jinja2" %});
        {% endwith %}
        {% endfor %}

        {{ code_identifier }}_writer.close();
    }
}
