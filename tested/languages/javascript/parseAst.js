const { parse } = require('abstract-syntax-tree');
const fs = require('fs');
const source = fs.readFileSync(process.argv[2], 'utf-8');
const ast = parse(source).body;

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
    } else {
        return [];
    }
}

const array = JSON.stringify(ast.flatMap(mapSubTreeToIds).flatMap(mapIdToName));
console.log(array.substring(1, array.length - 1));
