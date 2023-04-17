using System.Collections.Generic;

class Submission {
    public static void Main(string[] args) {
        List<object> list = new List<object> { "strintg", 20 };
        foreach (object item in list) {
            string i = (string) item;
        }
    }
}
