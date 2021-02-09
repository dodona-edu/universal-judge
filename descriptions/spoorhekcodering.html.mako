<p>
    In the <span style="font-style: italic;">rail fence cipher</span>
    (also called <span style="font-style: italic;">zigzag cipher<span>),
    the letters of the plaintext are initially written downwards and diagonally on successive "rails" of an imaginary
    fence, and then moving up after the bottom rail has been reached. When the top rail is reached,
    the message is again written downwards until the whole plaintext is written out.
    If the text "<code>And now for something completely different.</code>"
    is written as such across four rails, we get the following result
</p>

<div class="highlighter-rouge language-python">
<pre class="highlight">
<code>
<span style="color:#888888;"><span style="background-color:#ffff00; color:#000000;"><strong>A</strong></span>#####<span style="background-color:#ffff00; color:#000000;"><strong>w</strong></span>#####<span style="background-color:#ffff00; color:#000000;"><strong>s</strong></span>#####<span style="background-color:#ffff00; color:#000000;"><strong>i</strong></span>#####<span style="background-color:#ffff00; color:#000000;"><strong>m</strong></span>#####<span style="background-color:#ffff00; color:#000000;"><strong>l</strong></span>#####<span style="background-color:#ffff00; color:#000000;"><strong>f</strong></span>#####<span style="background-color:#ffff00; color:#000000;"><strong>.</strong></span></span>
<span style="color:#888888;">#<span style="background-color:#ffff00; color:#000000;"><strong>n</strong></span>###<span style="background-color:#ffff00; color:#000000;"><strong>o</strong></span>#<span style="background-color:#ffff00; color:#000000;"> </span>###<span style="background-color:#ffff00; color:#000000;"> </span>#<span style="background-color:#ffff00; color:#000000;"><strong>o</strong></span>###<span style="background-color:#ffff00; color:#000000;"><strong>h</strong></span>#<span style="background-color:#ffff00; color:#000000;"><strong>n</strong></span>###<span style="background-color:#ffff00; color:#000000;"><strong>o</strong></span>#<span style="background-color:#ffff00; color:#000000;"><strong>p</strong></span>###<span style="background-color:#ffff00; color:#000000;"><strong>e</strong></span>#<span style="background-color:#ffff00; color:#000000;"><strong>y</strong></span>###<span style="background-color:#ffff00; color:#000000;"><strong>f</strong></span>#<span style="background-color:#ffff00; color:#000000;"><strong>e</strong></span>###<span style="background-color:#ffff00; color:#000000;"><strong>t</strong></span>#</span>
<span style="color:#888888;">##<span style="background-color:#ffff00; color:#000000;"><strong>d</strong></span>#<span style="background-color:#ffff00; color:#000000;"><strong>n</strong></span>###<span style="background-color:#ffff00; color:#000000;"><strong>f</strong></span>#<span style="background-color:#ffff00; color:#000000;"><strong>r</strong></span>###<span style="background-color:#ffff00; color:#000000;"><strong>m</strong></span>#<span style="background-color:#ffff00; color:#000000;"><strong>t</strong></span>###<span style="background-color:#ffff00; color:#000000;"><strong>g</strong></span>#<span style="background-color:#ffff00; color:#000000;"><strong>c</strong></span>###<span style="background-color:#ffff00; color:#000000;"><strong>l</strong></span>#<span style="background-color:#ffff00; color:#000000;"><strong>t</strong></span>###<span style="background-color:#ffff00; color:#000000;"> </span>#<span style="background-color:#ffff00; color:#000000;"><strong>i</strong></span>###<span style="background-color:#ffff00; color:#000000;"><strong>r</strong></span>#<span style="background-color:#ffff00; color:#000000;"><strong>n</strong></span>##</span>
<span style="color:#888888;">###<span style="background-color:#ffff00; color:#000000;"> </span>#####<span style="background-color:#ffff00; color:#000000;"><strong>o</strong></span>#####<span style="background-color:#ffff00; color:#000000;"><strong>e</strong></span>#####<span style="background-color:#ffff00; color:#000000;"> </span>#####<span style="background-color:#ffff00; color:#000000;"><strong>e</strong></span>#####<span style="background-color:#ffff00; color:#000000;"><strong>d</strong></span>#####<span style="background-color:#ffff00; color:#000000;"><strong>e</strong></span>###</span></code> </pre>
</div>

<p>
    The encoded message is then formed by reading the letters on each rail from left to right,
    and going through the rails top to bottom. The encoded message for the above example thus reads as
    "<code>Awsimlf.no  ohnopeyfetdnfrmtgclt irn oe ede</code>".
</p>

<h3>Assignment</h3>
<ul>
    <li>Write a function <code>${get_function_name("encode")}</code> that takes two arguments:
        <ol>
            <li>a text string (${get_type_name("text")}) and</li>
            <li>the number (${get_type_name("integer")}) of rails used in the rail fence cipher.</li>
        </ol>
        The function must return a string (${get_type_name("text")}) containing the encoded message of the given text,
        according to the rail fence cipher with the given number of rails.
    </li>
    <li>Write a function <code>${get_function_name("decode")}</code> that takes two arguments:
        <ol>
            <li> a text (${get_type_name("text")}) encoded according to the rail fence cipher and </li>
            <li>the number (${get_type_name("integer")}) of rails used in the coding scheme.</li>
        </ol>
        The function must return a string (${get_type_name("text")}) containing the original text after decoding.
    </li>
</ul>

${get_appendix()}

<h3>Example</h3>

${get_code_start()}\
${get_code('encode("And now for something completely different.", 1)', statement=True)}
${get_code('"And now for something completely different."')}
${get_code('encode("And now for something completely different.", 2)', statement=True)}
${get_code('"Adnwfrsmtigcmltl ifrn.n o o oehn opeeydfeet"')}
${get_code('encode("And now for something completely different.", 3)', statement=True)}
${get_code('"Anfstgmt fnn o o oehn opeeydfeetdwrmicllir."')}
${get_code('encode("And now for something completely different.", 4)', statement=True)}
${get_code('"Awsimlf.no  ohnopeyfetdnfrmtgclt irn oe ede"')}
${get_code('encode("And now for something completely different.", 5)', statement=True)}
${get_code('"Aftm nn oehopydetdwrmicllir. o on eefensgtf"')}

${get_code('decode("And now for something completely different.", 1)', statement=True)}
${get_code('"And now for something completely different."')}
${get_code('decode("Adnwfrsmtigcmltl ifrn.n o o oehn opeeydfeet", 2)', statement=True)}
${get_code('"And now for something completely different."')}
${get_code('decode("Anfstgmt fnn o o oehn opeeydfeetdwrmicllir.", 3)', statement=True)}
${get_code('"And now for something completely different."')}
${get_code('decode("Awsimlf.no  ohnopeyfetdnfrmtgclt irn oe ede", 4)', statement=True)}
${get_code('"And now for something completely different."')}
${get_code('decode("Aftm nn oehopydetdwrmicllir. o on eefensgtf", 5)', statement=True)}
${get_code('"And now for something completely different."')}\
${get_code_end()}