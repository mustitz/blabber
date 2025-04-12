using System;

class MyMaybe<T>
{
}

class MyNothing<T> : MyMaybe<T>
{
    public MyNothing() {}
}

class MyJust<T> : MyMaybe<T>
{
    public T Value { get; init; }
    public MyJust(T value) { Value = value; }
}

class Program
{
    static String ShowMaybe<T>(MyMaybe<T> obj)
    {
        if (obj is MyNothing<T>)
        {
            return "Nothing";
        }
        if (obj is MyJust<T> just)
        {
            return "Just: " + just.Value?.ToString();
        }
        return "???";
    }

    static void Main()
    {
        var A = new MyNothing<int>();
        var B = new MyJust<int>(123);
        Console.WriteLine("A = " + ShowMaybe(A));
        Console.WriteLine("B = " + ShowMaybe(B));
    }
}
