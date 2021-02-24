const fs = require("fs");

function row(boarding_pass) {
    return parseInt(
        boarding_pass.slice(0, 7).replace(/F/g, "0").replace(/B/g, "1"),
        2
    );
}

// console.log(row("FBFBBFFRLR"));
// 44
// console.log(row("BFFFBBFRRR"));
// 70
// console.log(row("FFFBBBFRRR"));
// 14
// console.log(row("BBFFBBFRLL"));
// 102

function column(boarding_pass) {
    return parseInt(
        boarding_pass.slice(7).replace(/L/g, "0").replace(/R/g, "1"),
        2
    );
}

// console.log(column("FBFBBFFRLR"));
// 5
// console.log(column("BFFFBBFRRR"));
// 7
// console.log(column("FFFBBBFRRR"));
// 7
// console.log(column("BBFFBBFRLL"));
// 4

function seatId(boarding_pass) {
    return 8 * row(boarding_pass) + column(boarding_pass);
}

// console.log(seatId("FBFBBFFRLR"));
// 357
// console.log(seatId("BFFFBBFRRR"));
// 567
// console.log(seatId("FFFBBBFRRR"));
// 119
// console.log(seatId("BBFFBBFRLL"));
// 820


function missingSeatId(boarding_passes) {

    // determine list of seat IDs
    const seats = [...fs.readFileSync(boarding_passes, "utf-8")
      .trim()
      .split("\n")
      .map(seat => seatId(seat.trim()))
    ];

    // sort seat IDs
    seats.sort((a, b) => a - b);

    for (let index = 0; index < seats.length - 1; index += 1) {
        if (seats[index] + 1 !== seats[index + 1]) {
            return seats[index] + 1;
        }
    }
}

// console.log(missingSeatId("boarding_list.txt"));
// 658
// console.log(missingSeatId("adventofcode.input.txt"));
// 562
