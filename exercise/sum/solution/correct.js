const getallen = process.argv.slice(1)
let som = 0

for (const getal of getallen) {
    const r = parseInt(getal)
    if (isNaN(r)) {
        console.error("som: ongeldige argumenten")
        process.exit(1);
    } else {
        som += r
    }
}

console.log(som);

