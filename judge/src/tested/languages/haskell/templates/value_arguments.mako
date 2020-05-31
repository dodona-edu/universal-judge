## Convert a list of arguments
<%page args="arguments" />\
% for item in arguments:
    <%include file="statement.mako" args="statement=item,lifting=False" />\
    % if not loop.last:
        , \
    % endif
% endfor