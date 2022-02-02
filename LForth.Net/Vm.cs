/** # Forth.Net
 * Forth is a mind expanding language. This is a VM that represents the state of a Forth program at a particular
 * point in time. In typical Forth fashion, it knows both how to translate Forth words to bytecode instruction and
 * how to execute such instructions to move forward the state, including creating new user defined instruction.
 *
 * If you are confused, you are welcome
 **/

namespace Forth;
using static Forth.Utils;

/** The VM accepts several opcodes, which have a fixed byte lenght (i.e., one byte) and can be followed
 * by parameters of variable lenght. The bytecode executor is in charge of moving the instruction pointer (IP)
 * forward the correct number of bytes depending on the instruction.
 **/

public enum Op {
    Colo, Semi, Does, Numb, Plus, Minu, Mult, Divi, Prin
}

/** Why is this a struct? No real reason. It would work as a class as well. I did measure perf of
 * calling an instance method on both class, struct and extension methods. It looks like struct instance
 * methods are a bit faster. This is the tight inner loop, so such small perf wins are welcome.
 **/
public struct Vm {

    /** Forth true and false bizarre definition. **/
    const Cell TRUE  = -1;
    const Cell FALSE = 0;

    /** Sizes of data types. **/
    const Index CHAR_SIZE = 1;
    const Index CELL_SIZE = sizeof(Cell);

    /** There are a lot of buffers in Forth that needs to be represented in data space, because
     * you can store their address on the stack. Read a Forth book to understand them.
     **/
    Index source;
    Index keyWord;
    Index word;
    Index inp;
    Index source_max_chars;
    Index word_max_chars;
    Index input_len_chars = 0;
    Index pad;
    Index strings;
    Index base_p;

    /** These are the parameter stack, return stack and data stack.
     *  Why not use the .NET stack class? Because we need access to the internal data structure underlying it.
     **/
    Index sp  = 0;
    Index rp  = 0;
    Index here_p  = 0;
    Cell[] ps;
    Cell[] rs;
    Cell[] ds;

    /** The code stack is separate from the data stack, differently than in classic Forth. This is mainly
     * because instruction opcodes are smaller than cells.They can be indexed more easily if kept separate.
     * I believe ANS Forth allows this. IP is the instruction pointer, while cp is the top of the stack.
     **/
    Index    ip = 0;
    Index    cp = 0;
    Code[]   cs;

    /** Words in Forth are pointers to code (and memory), plus metadata. They are kept in a list, which
     * makes retrieval slower, but allows multiple copies with the same name, which seems necessary at times.
     * Perhaps an hashtable woould work as well, and be faster, but staying safe for now.
     * Again, ANS Forth allows this, instead of the traditional way of keeping a linked list of Words in the
     * Data Space.
     **/
    record struct Word(string Name, Index Ip, Index Dsp, bool Immediate);
    List<Word>  dict;

    /** This is implemented as a callback, instead of one of the existing .net interfaces to read text
     * so that we can inject special behavior before of after reading the line from file or console.
     **/
    Func<string>? nextLine;

    public Vm(
        Func<string>? getLine   = null,
        Index maxParameterStack = Config.SmallStack,
        Index maxReturnStack    = Config.SmallStack,
        Index maxDataStack      = Config.SmallStack,
        Index maxDictionary     = Config.SmallStack,
        Index maxCodeStack      = Config.SmallStack,
        Index maxStrings        = 128,
        Index maxPad            = 1_024,
        Index maxSource         = 1_024,
        Index maxWord           = 128
        ) {

        nextLine = getLine;

        ps   = new Cell[maxParameterStack];
        rs   = new Cell[maxReturnStack];
        ds   = new Cell[maxDataStack];
        cs   = new Code[maxCodeStack];
        dict = new (maxDictionary);

        keyWord = here_p;
        here_p += maxWord * CHAR_SIZE;
        source  = here_p;
        here_p += maxSource * CHAR_SIZE;

        word    = here_p;
        here_p += maxWord * CHAR_SIZE;

        pad              = here_p;
        here_p           += maxPad;
        source_max_chars = maxSource;
        word_max_chars   = maxWord;

        base_p     = here_p;
        here_p    += CELL_SIZE;
        ds[base_p] = 10;

        strings = here_p;
        here_p += maxStrings * Vm.CHAR_SIZE;

        inp     = here_p;
        here_p += CELL_SIZE;
    }
    /** This is the main dynamic dispatch point. There is a large literature on how to do this faster.
     * It seems that some form of computed goto is necessary (as gcc allows) for maximum performance.
     * This is because of branch prediction in the cpu. With a switch statement you have just one jumping
     * point for the cpu to predict. By adding calculated goto into each case statement, you give the cpu
     * multiple decision point allowing better hit rate.
     *
     * TThe switch statement is translated to a jump table by the compiler, so you don't get better perf
     * by using an array of delegate. I have also benchmarked that.
     *
     * TThe expectation is to execute code from ip to the end of the code segment, represented by cp.
     **/
    public void Execute() {
        while(ip < cp) {
            var op = cs[ip];
            ip++;
            Cell n;
            switch((Op)op) {
                case Op.Numb:
                    n = Read7BitEncodedCell(cs, ip, out var count);
                    Push(n);
                    ip += count;
                    break;
                case Op.Prin:
                    n = Pop();
                    Console.Write($"{n} ");
                    break;
                default:
                    Throw($"{((Op)op).ToString()} bytecode not supported.");
                    break;
            }
        }
    }
    /** These are internal to be able to test them. What a bother. **/
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal void Push(Cell n)  => ps[sp++] = n;
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal Cell Pop()         => ps[sp--];

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal void RPush(Cell n) => rs[rp++] = n;
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal Cell RPop()        => rs[rp--];

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal void Comma(Cell n)    => ds[here_p++] = n;
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal Cell UnComma()        => ds[here_p--];

    /** Refill fills out the input buffer at source, translating from UTF16 (.net) to UTF8 (what I want).
     * It would be nice to have a separate segment for strings, but Forth requires them to be store in the
     * Data Store, despite that been an array of long/ints generally. That is to allow mixed data structures
     * containing both strings and values.
     **/
    void Refill()
    {
        if(nextLine == null) Throw("Trying to Refill, without having passed a nextLine func");

        var inputBuffer = nextLine!();

        if (inputBuffer == null)
        {
            Push(FALSE);
        }
        else
        {
            var len = inputBuffer.Length;
            if (len > source_max_chars)
                throw new Exception(
                $"Cannot parse a line longer than {source_max_chars}. {inputBuffer} is {len} chars long.");
            var inputCharSpan = ToChars(source, source_max_chars);
            var bytes = new Span<byte>(Encoding.UTF8.GetBytes(inputBuffer));
            bytes.CopyTo(inputCharSpan);
            ds[inp] = 0;
            input_len_chars = len;
            Push(TRUE);
        }
    }
    /*
    void wordW(ref Vm vm, bool inKeyword = false)
    {
        var delim = (char)pop(ref vm);
        var s = ToChars(ref vm, vm.source, vm.input_len_chars);
        var toPtr = inKeyword ? vm.keyWord : vm.word;

        var w = ToChars(ref vm, toPtr, vm.word_max_chars);

        var j = 1; // It is a counted string, the first 2 bytes contains the length

        ref var inp = ref vm.ds[vm.inp];

        while (inp < vm.input_len_chars && s[inp] == delim) { inp++; }

        // If all spaces to the end of the input, return a string with length 0. 
        if (inp >= vm.input_len_chars)
        {
            w[0] = (char)0;
            push(ref vm, toPtr);
            return;
        }

        // Copy chars until end of space allocated, end of buffer or delim.
        while (j < vm.word_max_chars && inp < vm.input_len_chars && s[inp] != delim)
        {
            var c = s[inp++];
            w[j++] = c;
        }
        // Points past the delimiter. Otherwise it would stay on last " of a string.
        if(inp < vm.input_len_chars) inp++;
        if (j >= vm.word_max_chars) throw new Exception($"Word longer than {vm.word_max_chars}: {s}");

        w[0] = (char)(j - 1);  // len goes into the first char
        push(ref vm, toPtr);
    }
    */
    Span<byte> ToChars(Index start, Index lenInChars) {
        var cellSpan = ds.AsSpan(start, lenInChars / CELL_SIZE + 1);
        return MemoryMarshal.Cast<Cell, byte>(cellSpan);
    }
}

static class Utils {
    static void Add<T>(this T[] a, Index i, T t) {
        if(a.Length == i) Array.Resize(ref a, i * 2);
        a[i] = t;
    }
    internal static void Throw(string message) => throw new Exception(message);

    internal static long Read7BitEncodedCell(Code[] codes, Index index, out Index howMany) {
        using var stream = new MemoryStream(codes, index, 10);
        howMany = (Index)stream.Position;
#if CELL32
        var result = stream.ReadVarInt32();
#else
        var result = stream.ReadVarInt64();
#endif
        howMany = (Index)stream.Position - howMany;
        return result;
    }

    internal static void Write7BitEncodedCell(Code[] codes, Index index, Cell value, out Index howMany) {
        using var stream = new MemoryStream(codes, index, 10);
        howMany = (Index)stream.Position;
        stream.WriteVarInt(value);
        howMany = (Index)stream.Position - howMany;
    }

}
