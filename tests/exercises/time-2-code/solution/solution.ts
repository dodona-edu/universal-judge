import * as fs from 'fs';
import * as readline from 'readline';

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

function load(filename: string): string {
    try {
        return fs.readFileSync(filename, 'utf8').trim();
    } catch (err) {
        return "";
    }
}

function save(user: string, filename: string): void {
    fs.writeFileSync(filename, user + "\n");
}

let user: string = load("datafile.txt");

if (user === "") {
    console.log("Hello, I don't believe we have met.");
    rl.question("", (name: string) => {
        save(name, "datafile.txt");
        console.log("Nice to meet you " + name + ".");
        rl.close();
    });
} else {
    console.log("It's good to see you again, " + user + ".");
    rl.close();
}
