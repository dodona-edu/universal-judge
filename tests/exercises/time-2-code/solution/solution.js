const fs = require('fs');
const readline = require('readline');

function load(filename) {
    try {
        return fs.readFileSync(filename, 'utf8').trim();
    } catch (err) {
        return "";
    }
}

function save(user, filename) {
    fs.writeFileSync(filename, `${user}\n`, 'utf8');
}

const user = load("datafile.txt");

if (user === "") {
    console.log("Hello, I don't believe we have met.");
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });

    // Empty string to wait silently for input
    rl.question("", (name) => {
        save(name, "datafile.txt");
        console.log(`Nice to meet you ${name}.`);
        rl.close();
    });
} else {
    console.log(`It's good to see you again, ${user}.`);
}
