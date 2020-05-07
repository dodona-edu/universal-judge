import values
import sys
value_file = open("EfuIWnT0o_values.txt", "w")
exception_file = open("EfuIWnT0o_exceptions.txt", "w")
def write_separator():
    value_file.write("--EfuIWnT0o-- SEP")
    exception_file.write("--EfuIWnT0o-- SEP")
    sys.stderr.write("--EfuIWnT0o-- SEP")
    sys.stdout.write("--EfuIWnT0o-- SEP")
    sys.stdout.flush()
    sys.stderr.flush()
    value_file.flush()
    exception_file.flush()
def send_value(value):
    values.send_value(value_file, value)
def send_exception(exception):
    values.send_exception(exception_file, exception)
def send_specific_value(r):
    values.send_evaluated(value_file, r)
def send_specific_exception(r):
    values.send_evaluated(exception_file, r)
try:
    write_separator()
    from submission import *
except Exception as e:
    raise e
write_separator()
try:
    send_value(echo('input-1'))
except Exception as e:
    send_exception(e)
else:
    send_exception(None)
write_separator()
try:
    send_value(echo('input-2'))
except Exception as e:
    send_exception(e)
else:
    send_exception(None)
value_file.close()
exception_file.close()