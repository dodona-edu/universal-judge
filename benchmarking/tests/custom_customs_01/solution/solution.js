const fs = require("fs");

function groupCount(forms) {
    return new Set(forms.match(/[a-z]/g)).size;
}

// console.log(groupCount("abc"));
// 3
// console.log(groupCount("a b c"));
// 3
// console.log(groupCount("ab ac"));
// 3
// console.log(groupCount("a a a a"));
// 1
// console.log(groupCount("b"));
// 1

function* groupForms(filename) {

    let form = [];
    for (line of fs.readFileSync(filename, "utf-8").split("\n")) {
        line = line.trim();
        if (line.length === 0) {
            yield form.join(" ");
            form = [];
        } else {
            form.push(line);
        }
    }

}

// console.log([...groupForms("forms.txt")]);
// [ 'abc', 'a b c', 'ab ac', 'a a a a', 'b' ]

function planeCount(filename) {
    return [...groupForms(filename)]
      .map(forms => groupCount(forms))
      .reduce((a, b) => a + b);
}

// console.log(planeCount("forms.txt"));
// 11
// console.log(planeCount("adventofcode.input.txt"));
// 6748
