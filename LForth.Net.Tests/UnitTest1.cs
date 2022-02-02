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
    }
    public static IEnumerable<object[]> GetData7Bit() =>
        new (long, long)[] { (0, 1), (-1, 1), (+1, 1), (-900, 2), (-100_000, 3), (long.MaxValue, 10), (long.MinValue, 10) }
        .Select(t => new object[] {t.Item1, t.Item2});
    public static IEnumerable<object[]> GetDataPush() =>
            new long[] {-1, 0, 1, 2, 3, long.MaxValue, long.MinValue}.Select(o => new object[] { o });
}
