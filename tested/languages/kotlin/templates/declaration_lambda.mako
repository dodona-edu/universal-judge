<%page args="tp" />\
(\
% if tp.parameter_types == []:
    ()\
% else:
    (\
    % for param_type in tp.parameter_types:
        <%include file="declaration.mako" args="tp=param_type,value=None,nullable=False"/>\
        % if not loop.last:
            , \
        % endif
    % endfor
    )\
% endif
 -> \
% if tp.return_type:
    <%include file="declaration.mako" args="tp=tp.return_type,value=None,nullable=False"/>\
% else:
    Unit\
% endif
)