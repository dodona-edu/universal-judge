function repair(expenses) {

    for (let index1 = 0; index1 < expenses.length - 1; index1 += 1) {
        let expenses1 = expenses[index1];
        for (let index2 = index1 + 1; index2 < expenses.length; index2 += 1) {
            let expenses2 = expenses[index2];
            if (expenses1 + expenses2 === 2020) {
                return expenses1 * expenses2;
            }
        }
    }

}

// console.log(repair([1721, 979, 979, 366, 299, 675, 1456]));
// 514579
