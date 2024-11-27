function isIsbn10(code) {

    function checkDigit(code) {
        let check = 0;
        for (let i = 0; i < 9; i++) {
            check += parseInt(code[i]) * (i + 1);
        }
        check %= 11;
        return check === 10 ? 'X' : check.toString();
    }

    if (code.length !== 10) {
        return false;
    }

    if (isNaN(Number(code.substring(0, 9)))) {
        return false;
    }

    return code[9] === checkDigit(code);
}


function isIsbn13(code) {

    function checkDigit(code) {
        let check = 0;
        for (let i = 0; i < 12; i++) {
            check += parseInt(code[i]) * (i % 2 === 0 ? 1 : 3);
        }
        return ((((10 - check) % 10) + 10) % 10).toString();
    }

    if (code.length !== 13) {
        return false;
    }

    if (isNaN(Number(code.substring(0, 12)))) {
        return false;
    }

    return code[12] === checkDigit(code);
}

function isIsbn(code, isbn13=true) {
    return isbn13 ? isIsbn13(code) : isIsbn10(code);
}

function areIsbn(codes, isbn13=undefined) {
    if (isbn13 === undefined) {
        return codes.map((code) => typeof code === 'string' ? isIsbn(code, code.length === 13) : false);
    }
    return codes.map((code) => typeof code === 'string' ? isIsbn(code, isbn13) : false);
}
