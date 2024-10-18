const fs = require('fs');

function echoFile(content) {
    return new Promise((resolve, reject) => {
        fs.readFile(content, {encoding:'utf8', flag:'r'}, (err, data) => {
            if (err) {
                reject(err);
            } else {
                resolve(data);
            }
        });
    });
}
