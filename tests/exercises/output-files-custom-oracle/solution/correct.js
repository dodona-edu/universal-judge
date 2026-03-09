const fs = require('fs');

function generateEven() {
    fs.writeFileSync('even.txt', '10\n8\n6\n4\n2\n');
}

module.exports = {generateEven};
