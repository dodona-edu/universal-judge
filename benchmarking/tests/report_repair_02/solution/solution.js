function repair(expenses) {

    for (let index1 = 0; index1 < expenses.length - 2; index1 += 1) {
        let expenses1 = expenses[index1];
        for (let index2 = index1 + 1; index2 < expenses.length - 1; index2 += 1) {
            let expenses2 = expenses[index2];
            for (let index3 = index2 + 1; index3 < expenses.length; index3 += 1) {
                let expenses3 = expenses[index3];
                if (expenses1 + expenses2 + expenses3 === 2020) {
                    return expenses1 * expenses2 * expenses3;
                }
            }
        }
    }

}

// console.log(repair([1721, 979, 979, 366, 299, 675, 1456]));
// 241861950
