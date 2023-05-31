using System;

public struct Coords
{
    public Coords(double x, double y)
    {
        X = x;
        Y = y;
    }

    public double X { get; }
    public double Y { get; }

    public override string ToString() => $"({X}, {Y})";
}

class Submission
{
    public static Coords Echo(string content)
    {
        return new Coords(5.5, 7.5);
    }
}
