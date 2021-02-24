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

// console.log(seat_id("FBFBBFFRLR"));
// 357
// console.log(seat_id("BFFFBBFRRR"));
// 567
// console.log(seat_id("FFFBBBFRRR"));
// 119
// console.log(seat_id("BBFFBBFRLL"));
// 820


function highestSeatId(boarding_passes) {
    return Math.max(
        ...fs.readFileSync(boarding_passes, "utf-8")
          .trim()
          .split("\n")
          .map(seat => seatId(seat.trim()))
    );
}

// console.log(highest_seat_id("boarding_list.txt"));
// 820
// console.log(highest_seat_id("adventofcode.input.txt"));
// 806

