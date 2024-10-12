function isIsbn10(code): boolean {

    function checkDigit(code: string) {
        let check: number = 0;
        for (let i = 0; i < code.length - 1; i++) {
            check += parseInt(code[i]) * (i + 1);
        }
        check %= 11;
        return check === 10 ? 'X' : check.toString();
    }

    if (typeof code !== 'string') {
        return false;
    }

    if (code.length !== 10) {
        return false;
    }

    if (! isNaN(Number(code.substring(0, 9)))) {
        return false;
    }

    return code[9] === checkDigit(code);
}


function isIsbn13(code): boolean {

    function checkDigit(code: string) {
        let check: number = 0;
        for (let i = 0; i < code.length - 1; i++) {
            check += parseInt(code[i]) * (i % 2 === 0 ? 1 : 3);
        }
        return ((10 - check) % 10).toString();
    }

    if (typeof code !== 'string') {
        return false;
    }

    if (code.length !== 13) {
        return false;
    }

    if (! isNaN(Number(code.substring(0, 12)))) {
        return false;
    }

    return code[12] === checkDigit(code);
}

function isIsbn(code, isbn13: boolean=true): boolean {
    return isbn13 ? isIsbn13(code) : isIsbn10(code);
}

function areIsbn<T>(codes: Array<T>, isbn13: boolean=true): Array<boolean> {
    return codes.map((code) => isIsbn(code, isbn13));
}
