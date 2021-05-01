class Counter {
    constructor() {
        this.counter = 0;
    }

    add() {
        this.counter += 1;
        return this;
    }

    get() {
        return this.counter;
    }
}
