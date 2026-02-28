const fs = require('fs');

function generateEven() {
    fs.writeFileSync('even.txt', '9\n8\n6\n4\n2\n');
}

module.exports = {generateEven};
