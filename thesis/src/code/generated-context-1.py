import values
import sys

value_file = open(r"Ogdcj0QzN_values.txt", "w")
exception_file = open(r"Ogdcj0QzN_exceptions.txt", "w")

def write_delimiter(delimiter):
    value_file.write(delimiter)
    exception_file.write(delimiter)

def send(value):
    values.send_value(value_file, value)

def send_exception(exception):
    values.send_exception(exception_file, exception)

def e_evaluate_main(value):
    send_exception(value)

def v_evaluate_0(value):
    send(value)

def e_evaluate_0(value):
    send_exception(value)

# Import code.
try:
    from submission import *
except Exception as e:
    raise e

# Context 0-0.
try:
    v_evaluate_0(    loterij(6, 15)    )
except Exception as e:
    e_evaluate_0(e)

sys.stderr.write("--Ogdcj0QzN-- SEP")
sys.stdout.write("--Ogdcj0QzN-- SEP")
write_delimiter("--Ogdcj0QzN-- SEP")

value_file.close()
exception_file.close()
