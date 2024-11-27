class Coord {

    public x: number;
    public y: number;

    constructor(x: number, y: number ) {
        this.x = x;
        this.y = y;
    }

}

function echo(content: unknown) {
    return new Coord(5, 7);
}
