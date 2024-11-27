// Test ObjectPattern
const {c, d} = {c: 5, d: 7};
// Test Array Pattern
let [a, b] = ["alpha", "beta"];
// Test normal variables
var x = 5, y = 6;
// Test first reassignment
x = y;

// Test function
function demoFunction() {
}

// Test simple class
class SimpleClass {
    constructor() {
    }
}

// Test class with static variables
class StaticClass extends SimpleClass {
    data = ["Static data"];
    constants;
}

// Test try-catch
function tryCatch() {
    try {
        let demo = 5;
    } catch {
        // Do nothing
    }
}

// Test async function
async function asyncFunction() {
    return 5;
}

// Test second Reassignment
x = 5;
// Assignment to not defined var
z = x + y;
