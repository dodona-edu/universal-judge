class Submission
{
    private static string Content
    {
        get;
        set => field = value.Trim();
    }

    public static string Echo(string content)
    {
        Content = content;
        return Content;
    }
}
