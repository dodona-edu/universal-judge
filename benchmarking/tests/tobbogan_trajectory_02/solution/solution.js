fs = require('fs');

function countTrees(x_offset, y_offset, file) {
    contents = fs.readFileSync(file, "utf-8");
    lines = contents.split("\n");

    var x_pos = 0;
    var y_pos = 0;
    var tree_count = 0;
    while (y_pos < lines.length) {
        if(lines[y_pos][x_pos] == "#")
        {
            tree_count++;
        }
        x_pos = (x_pos + x_offset) % (lines[y_pos].length);
        y_pos = (y_pos + y_offset)
    }
    return tree_count;
}

