int apply(int (*f)(int, int), int a, int b) {
    return (*f)(a, b);
}

int add(int a, int b) {
    return a + b;
}
