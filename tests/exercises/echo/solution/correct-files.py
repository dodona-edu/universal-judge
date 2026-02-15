i = input()

try:
    file_txt = open("input2.txt", "r").read()
except FileNotFoundError:
    file_txt = None

if file_txt:
    print(file_txt.strip())
else:
    print(i)
