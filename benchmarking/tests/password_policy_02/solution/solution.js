function isValidPassword(password, policy) {

    // split policy into components
    const [first, second, letter] = policy.match(/(\d+)-(\d+) ([a-z])/).slice(1, 4);

    // check policy
    return Boolean(
        password[parseInt(first) - 1] === letter ^
        password[parseInt(second) - 1] === letter
    );

}

// console.log(isValidPassword("abcde", "1-3 a"));
// true
// console.log(isValidPassword("cdefg", "1-3 b"));
// false
// console.log(isValidPassword("ccccccccc", "2-9 c"));
// false
