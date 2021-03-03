const fs = require("fs");

function intersection(set1, set2) {
    return new Set([...set1].filter(x => set2.has(x)));
}

function groupCount(forms) {
    let all;
    for (let form of forms.split(" ")) {
        form = new Set(form.match(/[a-z]/g));
        all = (all === undefined) ? form : intersection(all, form);
    }
    return all.size;
}

// console.log(groupCount("abc"));
// 3
// console.log(groupCount("a b c"));
// 0
// console.log(groupCount("ab ac"));
// 1
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
// 6
// console.log(planeCount("adventofcode.input.txt"));
// 3445
