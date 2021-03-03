const fs = require('fs');

function countTrees(f) {
  const file = fs.readFileSync(f, "utf-8");;
  const lines = file.split("\n");
  return trees(lines, 3, 1);
}

function trees(lines, right, down) {
    const rowLength = lines[0].length;
    return new Array(Math.floor(lines.length / down)).fill(null)
      .filter((_, i) => lines[down * i].charAt(right * i % rowLength) === '#')
      .length;
}
