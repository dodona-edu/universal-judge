import * as fs from 'fs';

const input = fs.readFileSync(0, 'utf8').split('\n')[0];

if (fs.existsSync('input2.txt')) {
    const fileTxt = fs.readFileSync('input2.txt', 'utf8').trim();
    if (fileTxt) {
        console.log(fileTxt);
    } else {
        console.log(input);
    }
} else {
    console.log(input);
}
