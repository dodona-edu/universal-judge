import time

def my_func2():
    a = [1] * (10 ** 6)
    b = [2] * (2 * 10 ** 7)
    del b
    return a

def my_func():
    my_func2()
    time.sleep(0.1)
    c = my_func2()
    time.sleep(0.1)
    my_func2()
    time.sleep(0.1)
    del c
    time.sleep(0.1)
    my_func2()
    time.sleep(0.1)