function loterij(aantal=6, maximum = 42) {
    const getallen = new Set();
    while (getallen.size < aantal) {
        getallen.add(Math.floor(Math.random() * maximum) + 1);
    }
    return Array.from(getallen).sort((x: number, y: number) => y - x).join(" - ");
}
