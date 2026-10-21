function add(a, b) {
    return a + b;
}

function greet(name) {
    throw new Error(`cannot greet ${name}`);
}

function printGreeting(name) {
    console.log(`Hello, ${name}!`);
}
