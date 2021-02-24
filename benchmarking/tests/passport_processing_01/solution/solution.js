const fs = require('fs');

function countValidPassports(f) {
  const file = fs.readFileSync(f, "utf-8");
  const passes = file.split("\n\n").map(p => p.trim());
  return passes.filter(pass => isValidPassport(pass.split("\n").join(" "))).length;
}

function isValidPassport(pass) {
  const fields = pass.split(" ");
  if (fields.length === 8) return true;
  if (fields.length === 7) return !fields.map(f => f.split(":")[0]).includes("cid");
  return false;
}
