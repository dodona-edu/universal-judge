const name === "???";

% for cont in contexts:
    if ("${cont}" === name) {

        import * from "${cont}.js";
        process.exit(${cont}());

    }
% endfor

## If we selected a context, we should have exited before.
console.error(`Non-existing selector ${name} selected.`);
process.exit(-1);
