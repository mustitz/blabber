using System;
using System.Collections.Generic;
using System.Linq;

public interface IFunctor<FA, FB, A, B>
{
    FB Map(Func<A, B> f, FA fa);
}

public class ListFunctor<A, B> : IFunctor<List<A>, List<B>, A, B>
{
    public List<B> Map(Func<A, B> f, List<A> fa)
    {
        return fa.Select(f).ToList();
    }
}

class Program
{
    static void Main()
    {
        var input = new List<string> { "Hello", "Functor", "Programming", "World" };
        var output = new ListFunctor<string, int>().Map(s => s.Length, input);

        Console.WriteLine("Input: " + string.Join(", ", input));
        Console.WriteLine("Output: " + string.Join(", ", output));
    }
}
