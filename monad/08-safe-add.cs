using System;

class Program
{
    static int? SafeAdd(int? a, int? b)
    {
        if (a == null) {
            return null;
        }

        if (b == null)
            return null;

        return a.Value + b.Value;
    }

    static string AsStr(int ?a)
    {
        return a?.ToString() ?? "null";
    }

    static void Test(int? a, int ?b)
    {
        var sa = AsStr(a);
        var sb = AsStr(b);
        var sc = AsStr(SafeAdd(a, b));
        Console.WriteLine($"SafeAdd({sa}, {sb}) = {sc}");
    }

    static void Main()
    {
        Test(5, 7);
        Test(null, 7);
        Test(5, null);
        Test(null, null);
    }
}
