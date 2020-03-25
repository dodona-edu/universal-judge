## Convert a list of arguments
<%page args="arguments" />
% for item in arguments:
    <%include file="expression.mako" args="expression=item" />
    % if not loop.last:
        , \
    % endif
% endfor
