const fs = require("fs");

class Counter {
    constructor() {
        this.counter = 0;
    }

    add() {
        this.counter += 1;
    }

    get() {
        var x;
        return this.counter;
    }
}
