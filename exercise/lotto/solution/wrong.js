function loterij(aantal=6, maximum = 42) {
    const getallen = new Set();
    while (getallen.size < aantal) {
        getallen.add(Math.floor(Math.random() * maximum) + 1);
    }
    return [...getallen].sort((x, y) => y - x).join(" - ");
}
