import os
import tempfile


def echo_function(filename, string_to_write):
    # mkstemp creates the file with mode 0600.
    descriptor, temporary = tempfile.mkstemp(dir=".")
    with os.fdopen(descriptor, "w") as file:
        file.write(string_to_write + "\n")
    os.replace(temporary, filename)
