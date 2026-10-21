function add(a: number, b: number): number {
    return a + b;
}

function greet(name: string): string {
    throw new Error(`cannot greet ${name}`);
}

function printGreeting(name: string): void {
    console.log(`Hello, ${name}!`);
}
