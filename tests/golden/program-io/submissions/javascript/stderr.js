const fs = require("fs");

console.error(`debug: ${process.argv.length - 1} arguments`);

const [prefix, ...names] = process.argv.slice(1);
if (prefix === undefined) {
    process.exit(1);
}

const lines = fs.readFileSync(0, "utf-8").split("\n").filter((line) => line);
for (const name of [...names, ...lines]) {
    console.log(`${prefix}: ${name}`);
}
