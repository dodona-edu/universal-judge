import * as ts from 'typescript';
import * as fs from 'fs';
const source = fs.readFileSync(process.argv[2], 'utf-8');

const ast = ts.createSourceFile(
    process.argv[2], // File name
    source,          // Source code
    ts.ScriptTarget.ESNext, // Target language version
    true             // SetParentNodes option to preserve parent-child relationships
);

// Helper function to extract relevant identifiers from AST nodes
function mapSubTreeToIds(node: ts.Node): Array<ts.Node|undefined> {
    // Make sure that no definitions inside a block of some kind are accounted with.
    if (ts.isForOfStatement(node) || ts.isForInStatement(node) || ts.isIfStatement(node) ||
        ts.isForStatement(node) || ts.isWhileStatement(node) || ts.isBlock(node)) {
        return []
    }

    if (ts.isVariableDeclaration(node)) {
        return [node.name];
    } else if (ts.isFunctionDeclaration(node) || ts.isClassDeclaration(node)) {
        return [node.name];
    } else if (ts.isExpressionStatement(node) &&
        ts.isBinaryExpression(node.expression) &&
        node.expression.operatorToken.kind === ts.SyntaxKind.EqualsToken) {
        return [node.expression.left];
    } else {
        const ids: Array<ts.Node| undefined> = [];
        ts.forEachChild(node, (child: ts.Node) => {
            ids.push(...mapSubTreeToIds(child));
        });
        return ids;
    }
}

// Convert node to identifier names, handling patterns (Array/Object destructuring)
function mapIdToName(node: ts.Node|undefined): Array<any> {
    if (!node) {
        return [];
    }

    if (ts.isIdentifier(node)) {
        return [node.text];
    } else if (ts.isArrayBindingPattern(node)) {
        return node.elements.flatMap(element => ts.isBindingElement(element) ? mapIdToName(element.name) : []);
    } else if (ts.isObjectBindingPattern(node)) {
        return node.elements.flatMap(prop => mapIdToName(prop.name));
    } else {
        return [];
    }
}

try {
    const array = Array.from(new Set(mapSubTreeToIds(ast).flatMap(mapIdToName)));
    console.log(array.join(', '));
} catch (e) {
    // Assume this is invalid TypeScript at this point.
    console.error(e);
    process.exit(0);
}
