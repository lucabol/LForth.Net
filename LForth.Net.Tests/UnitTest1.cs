using Xunit;
using Forth;
using System.Collections.Generic;
using System.Linq;
using static Forth.Utils;

namespace LForth.Net.Tests;

public class UnitTest1
{
    [Theory]
    [MemberData(nameof(GetData7Bit))]
    public void CanDoBit7Conversion(long n, long many) {
        var array = new byte[10];
        Write7BitEncodedCell(array, 0, n, out var howManyRead);
        var n1 = Read7BitEncodedCell(array, 0, out var howManyWritten);
        Assert.Equal(n, n1);
        Assert.Equal(many, howManyRead);
        Assert.Equal(many, howManyWritten);
    }
    [Theory]
    [MemberData(nameof(GetDataPush))]
    public void PushAndPopWork(long n) {
        var vm = new Vm();
        vm.Push(n);
        Assert.Equal(n, vm.Pop());
        vm.Push(n);
        vm.Push(0);
        vm.Pop();
        Assert.Equal(n, vm.Pop());
    }
    [Fact]
    public void ResizeStackWorks()
    {
        Vm vm = new(maxParameterStack: 10);
        for (long i = 0; i < 20; i++)
            vm.Push(i);
        for (long i = 19; i <0; i--)
            Assert.Equal(i, vm.Pop());
    }
    [Theory]
    [MemberData(nameof(GetDataRefill))]
    public void RefillWorks(string s)
    {
        var vm = new Vm(() => s);
        vm.Refill();
        var flag = vm.Pop();
        Assert.Equal(-1, flag);
        Assert.Equal(s, vm.InputBufferToString());
    }
    [Theory]
    [MemberData(nameof(GetDataWord))]
    public void WordWorks(string s, string[] words)
    {
        var vm = new Vm(() => s);
        vm.Refill();
        var flag = vm.Pop();
        Assert.Equal(-1, flag);
        Assert.Equal(s, vm.InputBufferToString());
    }
    public static IEnumerable<object[]> GetData7Bit() =>
        new (long, long)[] { (0, 1), (-1, 1), (+1, 1), (-900, 2), (-100_000, 3), (long.MaxValue, 10), (long.MinValue, 10) }
        .Select(t => new object[] {t.Item1, t.Item2});
    public static IEnumerable<object[]> GetDataPush() =>
            new long[] {-1, 0, 1, 2, 3, long.MaxValue, long.MinValue}.Select(o => new object[] { o });
    public static IEnumerable<object[]> GetDataRefill() =>
            new string[] {"", "a", " ab bb "}.Select(o => new object[] { o });
    public static IEnumerable<object[]> GetDataWord() =>
            new (string, string[])[] {("",new string[] {"" }), ("a", new string[]{"a"}), (" ab bb ", new string[] {"ab", "bb"})}.Select(t => new object[] { t.Item1, t.Item2 });
}
