const fs = require('fs');

function countValidPassports(f) {
  const file = fs.readFileSync(f, "utf-8");
  const passes = file.split("\n\n").map(p => p.trim());
  return passes.filter(pass => isValidPassport(pass.split("\n").join(" "))).length;
}

function isValidPassport(pass) {
  return pass.split(" ")
        .map(f => f.split(":"))
        .filter(f => f[0] !== "cid")
        .filter(f => isValidField(f[0], f[1]))
        .length === 7;
}

function isValidField(field, value) {
    if (field === "byr") return +value >= 1920 && +value <= 2002;
    if (field === "iyr") return +value >= 2010 && +value <= 2020;
    if (field === "eyr") return value.length === 4 && +value >= 2020 && +value <= 2030;
    if (field === "hcl") return null !== value.match(/^#[0-9a-f]{6}$/);
    if (field === "ecl") return ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].includes(value);
    if (field === "pid") return null !== value.match(/^[0-9]{9}$/);
    if (field === "hgt") {
      const v = value.substr(0, value.length-2);
      if (value.match(/cm$/)) {
        return +v >= 150 && +v <= 193;
      } else if (value.match(/in$/)) {
        return +v >= 59 && +v <= 76;
      }
      return false;
    };
}
