import * as fs from 'fs';

function echoFunction(filename, stringToWrite) {
    fs.writeFileSync(filename, stringToWrite + '\n', { flag: 'w' });
}
