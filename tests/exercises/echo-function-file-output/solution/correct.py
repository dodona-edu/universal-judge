def echo_function(filename, string_to_write):
    with open(filename, 'w') as file:
        file.write(string_to_write + '\n')  # Write the string and a newline)
