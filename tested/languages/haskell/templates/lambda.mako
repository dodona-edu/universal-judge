## This generates a lambda's in JavaScript.
<%page args="lambda_expr" />\
\\
% if not lambda_expr.parameters:
    ()\
% elif len(lambda_expr.parameters) == 1 and isinstance(lambda_expr.parameters[0], str):
    ${lambda_expr.parameters[0]}\
% elif isinstance(lambda_expr.parameters[0], str):
    % for argument in lambda_expr.parameters:
        ${argument}\
        % if not loop.last:
             \
        % endif
    % endfor
% else:
    % for argument in lambda_expr.parameters:
        ${argument.name}\
        % if not loop.last:
             \
        % endif
    % endfor
% endif
 -> \
<%include file="statement.mako" args="statement=lambda_expr.body"/>