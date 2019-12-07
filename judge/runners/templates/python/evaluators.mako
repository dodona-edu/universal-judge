import values

value_file = None
exception_file = None


def open_outputs():
    global value_file, exception_file
    value_file = open(r"${value_file}", "w")
    exception_file = open(r"${exception_file}", "w")


def close_outputs():
    value_file.close()
    exception_file.close()


def write_delimiter(delimiter):
    value_file.write(delimiter)
    exception_file.write(delimiter)


def evaluated(result, expected, actual, messages=[]):
    values.send_evaluated(value_file, result, expected, actual, messages)


def send(value):
    values.send_value(value_file, value)


def send_exception(exception):
    values.send_exception(exception_file, exception)


${main_testcase.exception_code}


% for additional in additional_testcases:
    % if additional.has_return:
        ${additional.value_code}
    % endif

    ${additional.exception_code}

% endfor
