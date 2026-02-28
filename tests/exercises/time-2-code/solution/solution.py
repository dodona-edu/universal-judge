# -------------------------
# Subprograms
# -------------------------
# Load a single line of data
def load(filename):
    file = open(filename, "r")
    user = file.read()
    file.close()
    user = user.strip()
    return user


# Save a single line of data
def save(user, filename):
    file = open(filename, "w")
    user = user + "\n"
    file.write(user)
    file.close()


# -------------------------
# Main program
# -------------------------
user = load("datafile.txt")
# If the file is empty, ask for the data to save...
if user == "":
    print("Hello, I don't believe we have met.")
    user = input("What is your name? ")
    save(user, "datafile.txt")
    print("Nice to meet you", user + ".")
# ...otherwise display the data in the file
else:
    print("It's good to see you again,", user + ".")
