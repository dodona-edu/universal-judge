words = []
for word in open('words.txt', 'r'):
    if ' ' not in word.strip():
        words.append(word.strip())
    
print(tuple(words))
