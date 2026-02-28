import * as fs from 'fs';

function generateEven(): void {
    fs.writeFileSync('even.txt', '10\n8\n6\n4\n2\n');
}
