function numbers(): number[] { return [1, 2, 3]; }
function total(xs: number[]): number { return xs.reduce((a, b) => a + b, 0); }
