import * as fs from 'fs';

function echoFile(content) {
    return fs.readFileSync(content, {encoding:'utf8', flag:'r'}).trim();
}
