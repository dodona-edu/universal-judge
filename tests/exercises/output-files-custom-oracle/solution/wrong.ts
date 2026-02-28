import * as fs from 'fs';

function generateEven(): void {
    fs.writeFileSync('even.txt', '9\n8\n6\n4\n2\n');
}
