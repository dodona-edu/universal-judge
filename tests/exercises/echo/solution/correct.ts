import * as fs from 'fs';
const stdinBuffer = fs.readFileSync(0); // STDIN_FILENO = 0
console.log(stdinBuffer.toString().trimEnd());
