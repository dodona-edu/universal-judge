function isValidPassword(password, policy) {

    // split policy into components
    const [lower, upper, letter] = policy.match(/(\d+)-(\d+) ([a-z])/).slice(1, 4);

    // count the number of occurrences of the letter
    let count = 0;
    for (let char of password) {
        if (char === letter) {
            count += 1;
        }
    }

    // check policy
    return parseInt(lower) <= count && count <= parseInt(upper);

}
