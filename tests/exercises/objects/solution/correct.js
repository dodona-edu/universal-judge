class EqualChecker {
    constructor(number) {
        this.number = number;
    }

    check(other) {
        return other === this.number;
    }
}

function setTest() {
    return new Set([[1, 2], [2, 3]]);
}
