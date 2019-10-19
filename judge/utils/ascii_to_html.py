import re

# TODO: MIT license, is this allowed?

ANSI2HTML_CODES_RE = re.compile('(?:\033\\[(\d+(?:;\d+)*)?([cnRhlABCDfsurgKJipm]))')
COLOURS = ['#000000', '#AA0000', '#00AA00', '#AA5500', '#0000AA', '#AA00AA', '#00AAAA', '#AAAAAA', '#555555',
           '#FF5555', '#55FF55', '#FFFF55', '#5555FF', '#FF55FF', '#55FFFF', '#FFFFFF']


def _ansi2html_get_styles():
    p = COLOURS

    regular_style = {
        '1': '',  # bold
        '2': 'opacity:0.5',
        '4': 'text-decoration:underline',
        '5': 'font-weight:bold',
        '7': '',
        '8': 'display:none',
    }
    bold_style = regular_style.copy()
    for i in range(8):
        regular_style['3%s' % i] = 'color:%s' % p[i]
        regular_style['4%s' % i] = 'background-color:%s' % p[i]

        bold_style['3%s' % i] = 'color:%s' % p[i + 8]
        bold_style['4%s' % i] = 'background-color:%s' % p[i + 8]

    # The default xterm 256 colour p:
    indexed_style = {}
    for i in range(16):
        indexed_style['%s' % i] = p[i]

    for rr in range(6):
        for gg in range(6):
            for bb in range(6):
                i = 16 + rr * 36 + gg * 6 + bb
                r = (rr * 40 + 55) if rr else 0
                g = (gg * 40 + 55) if gg else 0
                b = (bb * 40 + 55) if bb else 0
                indexed_style['%s' % i] = ''.join('%02X' % c if 0 <= c <= 255 else None for c in (r, g, b))

    for g in range(24):
        i = g + 232
        l = g * 10 + 8
        indexed_style['%s' % i] = ''.join('%02X' % c if 0 <= c <= 255 else None for c in (l, l, l))

    return regular_style, bold_style, indexed_style


def ansi2html(text):
    def _ansi2html(m):
        if m.group(2) != 'm':
            return ''
        import sys
        state = None
        sub = ''
        cs = m.group(1)
        cs = cs.strip() if cs else ''
        for c in cs.split(';'):
            c = c.strip().lstrip('0') or '0'
            if c == '0':
                while stack:
                    sub += '</span>'
                    stack.pop()
            elif c in ('38', '48'):
                extra = [c]
                state = 'extra'
            elif state == 'extra':
                if c == '5':
                    state = 'idx'
                elif c == '2':
                    state = 'r'
            elif state:
                if state == 'idx':
                    extra.append(c)
                    state = None
                    # 256 colors
                    color = indexed_style.get(c)  # TODO: convert index to RGB!
                    if color is not None:
                        sub += '<span style="%s:%s">' % ('color' if extra[0] == '38' else 'background-color', color)
                        stack.append(extra)
                elif state in ('r', 'g', 'b'):
                    extra.append(c)
                    if state == 'r':
                        state = 'g'
                    elif state == 'g':
                        state = 'b'
                    else:
                        state = None
                        try:
                            color = '#' + ''.join(
                                '%02X' % c if 0 <= c <= 255 else None for x in extra for c in [int(x)])
                        except (ValueError, TypeError):
                            pass
                        else:
                            sub += '<span style="%s:%s">' % ('color' if extra[0] == '38' else 'background-color', color)
                            stack.append(extra)
            else:
                if '1' in stack:
                    style = bold_style.get(c)
                else:
                    style = regular_style.get(c)
                if style is not None:
                    sub += '<span style="%s">' % style
                    stack.append(
                        c)  # Still needs to be added to the stack even if style is empty (so it can check '1' in stack above, for example)
        return sub

    stack = []
    regular_style, bold_style, indexed_style = _ansi2html_get_styles()
    sub = ANSI2HTML_CODES_RE.sub(_ansi2html, text)
    while stack:
        sub += '</span>'
        stack.pop()
    return sub
