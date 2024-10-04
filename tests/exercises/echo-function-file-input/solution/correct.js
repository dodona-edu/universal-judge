const fs = require('fs');

function echoFile(content) {
    return fs.readFileSync(content, {encoding:'utf8', flag:'r'});
}
