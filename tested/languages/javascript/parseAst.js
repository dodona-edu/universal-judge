const { parse } = require('abstract-syntax-tree');
const fs = require('fs');
const source = fs.readFileSync(process.argv[2], 'utf-8');

function mapSubTreeToIds(subtree) {
    const type = subtree.type;
    if (type === 'VariableDeclaration') {
        return subtree.declarations.map(row => row.id);
    } else if (type === 'FunctionDeclaration' || type === 'ClassDeclaration') {
        return [subtree.id];
    } else if (type === 'ExpressionStatement' &&
            subtree.expression.type === 'AssignmentExpression') {
        return [subtree.expression.left];
    } else {
        return [];
    }
}

function mapIdToName(id) {
    const type = id.type;
    if (type === 'Identifier') {
        return id.name;
    } else if (type === 'ArrayPattern') {
        return id.elements.flatMap(mapIdToName);
    } else if (type === 'ObjectPattern') {
        return id.properties.map(d => d.key).flatMap(mapIdToName);
    } else {
        return [];
    }
}

let ast;
try {
    // Add next option to support more JavaScript features.
    const ast = parse(source, {next: true}).body;
    // Use Set to remove duplicates
    const array = Array.from(new Set(ast.flatMap(mapSubTreeToIds).flatMap(mapIdToName)));
    console.log(array.join(', '));
} catch (e) {
    // Assume this is invalid JavaScript at this point.
    console.error(e);
    process.exit(0);
}
