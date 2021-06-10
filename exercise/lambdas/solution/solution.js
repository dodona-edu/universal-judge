class Store {
    constructor(a, b) {
        this.a = a;
        this.b = b;
    }

    apply(operator) {
        return operator(this.a, this.b)
    }
}

function createStore(constructor, a, b) {
    return new constructor(a, b);
}

function add(a, b) {
    return a + b;
}
