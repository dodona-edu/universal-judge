import values

value_file = None


def open_outputs():
    global value_file
    value_file = open(r"${output_file}", "w")


def close_outputs():
    value_file.close()


def value_write(value):
    value_file.write(value)


def evaluated(result, expected, actual, messages=[]):
    values.send_evaluated(value_file, result, expected, actual, messages)


def send(value):
    values.send_value(value_file, value)


% for additional in additionals:
    % if additional.has_return:
        ${additional.value_code}
    % endif

% endfor
