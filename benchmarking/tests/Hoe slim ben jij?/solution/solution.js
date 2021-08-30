const fs = require("fs");
// Getal inlezen
const buffer = fs.readFileSync(0, 'utf-8');

let cirkels = 0;

// Aantal cirkels in de cijfers van het getal tellen
for (let i = 0; i < buffer.length; i++) {
    let char = buffer.charAt(i);
    if (char === '8') {
        cirkels += 2;
    } else if (char === '0' || char === '4' || char === '6' || char === '9') {
        cirkels += 1;
    }
}

// Aantal cirkels uitschrijven
console.log(cirkels)