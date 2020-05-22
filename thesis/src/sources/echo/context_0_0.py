import values
import sys
value_file = open("Ft9AKVWPU_values.txt", "w")
exception_file = open("Ft9AKVWPU_exceptions.txt", "w")
def write_separator():
    value_file.write("--Ft9AKVWPU-- SEP")
    exception_file.write("--Ft9AKVWPU-- SEP")
    sys.stderr.write("--Ft9AKVWPU-- SEP")
    sys.stdout.write("--Ft9AKVWPU-- SEP")
    sys.stdout.flush()
    sys.stderr.flush()
    value_file.flush()
    exception_file.flush()
def send_value(value):
    values.send_value(value_file, value)
def send_exception(exception):
    values.send_exception(exception_file, exception)
def send_specific_value(value):
    values.send_evaluated(value_file, value)
def send_specific_exception(exception):
    values.send_evaluated(exception_file, exception)
try:
    write_separator()
    from submission import *
except Exception as e:
    send_exception(e)
else:
    send_exception(None)
value_file.close()
exception_file.close()