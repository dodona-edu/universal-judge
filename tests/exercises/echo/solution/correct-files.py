i = input()
file_txt = open("input2.txt", "r").read()

if file_txt:
    print(file_txt.strip())
else:
    print(i)
