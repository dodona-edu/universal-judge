module Context{{ context_id }} where

import qualified {{ name }}
import Values
import System.IO (hPutStr, stderr)

{# TODO: handle monads somehow?
      This will be difficult: if no stdout/stderr is expected, we will have to lift it into the monad.
-#}
main = do
    {#- In Haskell we do the before/after inside the main (?) #}
    {{ before or "" }}
    {#- Call main function #}
    {% with function=execution -%}
    {{ name }}.{%- include "function.jinja2" -%}
    {%- endwith %}
    {% for additional in additionals -%}
    hPutStr stderr "--{{ code_identifier }}-- SEP"
    putStr "--{{ code_identifier }}-- SEP"
    appendFile "{{ output_file }}" "--{{ code_identifier }}-- SEP"
    {%- with function=additional.input.function %}
    v{{ loop.index }} <- {{ name }}.{% include "function.jinja2" %}
    send v{{ loop.index }} "{{ output_file }}"
    {%- endwith -%}
    {%- endfor %}
    {{ after or "" }}
