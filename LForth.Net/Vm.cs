/** # Forth.Net
 * Forth is a mind expanding language. This is a VM that represents the state of a Forth program at a particular
 * point in time. In typical Forth fashion, it knows both how to translate Forth words to bytecode instruction and
 * how to execute such instructions to move forward the state, including creating new user defined instruction.
 *
 * If you are confused, you are welcome
 **/

namespace Forth;

using System.Diagnostics.CodeAnalysis;
using static Forth.Utils;

/** The VM accepts several opcodes, which have a fixed byte lenght (i.e., one byte) and can be followed
 * by parameters of variable lenght. The bytecode executor is in charge of moving the instruction pointer (IP)
 * forward the correct number of bytes depending on the instruction.
 **/

public enum Op {
    Colo, Semi, Does, Numb, Plus, Minu, Mult, Divi, Prin,
    Count, Word, Refill, Comma, Here, At, Store, State, Bl, Call, Dup, Nest, UnNest,
    Swap, Dup2, Drop, Drop2, Find
}

public class Vm {
    /** Enable debug to see the generated opcodes **/
    public bool Debug { get ; set; }

    /** Forth true and false bizarre definition. **/
    const Cell TRUE  = -1;
    const Cell FALSE = 0;

    /** Sizes of data types. **/
    internal const Index CHAR_SIZE = 1;
    internal const Index CELL_SIZE = sizeof(Cell);

    /** There are a lot of buffers in Forth that needs to be represented in data space, because
     * you can store their address on the stack. Read a Forth book to understand them.
     **/
    readonly Index source;
    readonly Index keyWord;
    readonly Index word;
    readonly Index inp;
    readonly Index source_max_chars;
    readonly Index word_max_chars;
    readonly Index pad;
    readonly Index strings;

    Index input_len_chars = 0;

    /** This is the base to interpret numbers. **/
    readonly Index base_p;
    /** State TRUE when compiling, FALSE when interpreting. **/
    readonly Index state;
    bool Executing { get => ReadCell (ds, state) == FALSE;
                     set => WriteCell(ds, state, value ? FALSE : TRUE);}

    /** These are the parameter stack, return stack and data stack.
     *  Why not use the .NET stack class? Because we need access to the internal data structure underlying it.
     **/
    internal Index sp  = 0;
    Index rp  = 0;
    Index herep  = 0;
    AUnit[] ps;
    AUnit[] rs;
    AUnit[] ds;

    /** The code stack is separate from the data stack, differently than in classic Forth. This is mainly
     * because instruction opcodes are smaller than cells.They can be indexed more easily if kept separate.
     * I believe ANS Forth allows this. IP is the instruction pointer, while cp is the top of the stack.
     **/
    Index    ip = 0;
    Index    cp = 0;
    readonly Code[]   cs;

    /** User defined words in Forth are pointers to code (and memory), plus metadata. They are kept in a list, which
     * makes retrieval slower, but allows multiple copies with the same name, which seems necessary at times.
     **/
    Index dictHead;

    /** There are some words that directly maps to single opcodes **/
    readonly Dictionary<string, Op> WordToSimpleOp = new()
    {
        { "."           , Op.Prin },
        { "count"       , Op.Count },
        { "refill"      , Op.Refill },
        { "word"        , Op.Word },
        { ","           , Op.Comma },
        { "here"        , Op.Here },
        { "@"           , Op.At },
        { "!"           , Op.Store },
        { "state"       , Op.State },
        { "bl"          , Op.Bl },
        { ":"           , Op.Colo },
        { ";"           , Op.Semi },
    };

    /** While other words need to perfom more complicated actions at compile time **/
    readonly Dictionary<string, Action> WordToDef = new();

    /** This is implemented as a callback, instead of one of the existing .net interfaces to read text
     * so that we can inject special behavior before of after reading the line from file or console.
     **/
    public Func<string>? NextLine = null;

    public Vm(
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

        ps   = new AUnit[maxParameterStack];
        rs   = new AUnit[maxReturnStack];
        ds   = new AUnit[maxDataStack];
        cs   = new Code[maxCodeStack];

        keyWord = herep;
        herep += maxWord * CHAR_SIZE;
        source  = herep;
        herep += maxSource * CHAR_SIZE;

        word    = herep;
        herep += maxWord * CHAR_SIZE;

        pad              = herep;
        herep          += maxPad;
        source_max_chars = maxSource;
        word_max_chars   = maxWord;

        base_p     = herep;
        herep    += CELL_SIZE;
        ds[base_p] = 10;

        strings = herep;
        herep += maxStrings * Vm.CHAR_SIZE;

        inp     = herep;
        herep += CELL_SIZE;

        state   = herep;
        herep += CELL_SIZE;

        WordToDef = new()
        {
            { "debug", () => Debug = !Debug },
            { "bye", () => Environment.Exit(0) },
        };
    }
    public void Reset()
    {
        sp = 0; rp = 0; Executing = true;
    }
    public void Quit()
    {
        if(NextLine is null) Throw("Trying to Quit with a null readline.");

        while(true)
        {
            Refill();
            if(Pop() == TRUE)
                Interpret();
            else
                break;
        }
    }
    public IEnumerable<string> Words()
    {
        return WordToSimpleOp.Keys.Concat(WordToDef.Keys);
    }
    void PushOp(Op op) => cs[cp++] = (Code)op;
    void PushOp(Op op, Cell value)
    {
        ip = cp;
        PushOp(op);
        Write7BitEncodedCell(cs, cp, value, out Index howMany);
        cp += howMany;
    }

    static Index LinkToCode(Index link, Index wordLen)
        // Addr + Link size + len size  + word chars
        => link + CELL_SIZE + CHAR_SIZE + CHAR_SIZE * wordLen;

    internal void Find()
    {
        var caddr = (Index)Pop();
        var clen  = ds[caddr];
        var cspan = new Span<AChar>(ds, caddr + 1 * CHAR_SIZE, clen);

        var dp = dictHead;
        while(true)
        {
            if(dp == 0) break;

            var wordNameStart = dp + CELL_SIZE;
            var wordLenRaw    = ds[wordNameStart];
            var wordLen       = Utils.ResetHighBit(wordLenRaw);
            var wordSpan      = new Span<AChar>(ds, wordNameStart + 1 * CHAR_SIZE, wordLen);
            var res           = cspan.SequenceEqual(wordSpan);
            if(res) // Found
            {
                Push(LinkToCode(dp, wordLen));                
                Push(Utils.HighBitValue(wordLenRaw) == 1 ? 1 : -1);
                return;
            }
            dp = (Index)ReadCell(ds, dp);
        }
        // Not found
        Push(caddr);
        Push(0);
    }
    /** Add Word to the dictionary. Assumes we are always adding at here.
    * The shape of the dictionary is |Link|Name Lenght Cell|Name chars|Code|
    * The Lenght cell has the higher bit set if an immediate word.
    **/
    internal void DictAdd()
    {
        // First put the link
        Push(dictHead);             // Push last index
        dictHead = herep;           // DH is now here
        Comma();                    // Store last index in new dictHead (here)

        // Copy word to here
        var len = ds[(Index)Peek()];
        Push(herep);
        Push(len);
        CMove();
        herep += len + 1;

    }
    void CMove()
    {
        var u  = Pop();
        var a2 = Pop();
        var a1 = Pop();
        Array.Copy(ds, a1, ds, a2, u + 1);
    }
    /** Interpreting means first compiling to the code stack, then executing the opcodes
     * if we are in Executing status. **/
    bool InterpretWord(string s)
    {
        var sl = s.ToLowerInvariant();
        Find();
        var res   = Pop();
        var xt    = Pop();

        if(res != 0)
        {
            PushOp(Op.Call, xt);
            var immediate = res == 1;
            if(Executing || immediate) Execute();
            return true;
        }
        if(WordToSimpleOp.TryGetValue(sl, out var op))
        {
            PushOp(op);
            if(Executing) Execute();
            return true;
        }
        if(WordToDef.TryGetValue(sl, out var def))
        {
            def();
            return true;
        }
        return false;
    }
    void InterpretNumber(Cell n)
    {
        PushOp(Op.Numb, n);
        if(Executing) Execute();
    }
    void Interpret()
    {
        while(true)
        {
            Bl();
            WordW();
            Count();
            var s = ToDotNetString();
            if(string.IsNullOrEmpty(s)) break;

            if(!InterpretWord(s))
                if(Cell.TryParse(s, out Cell n))
                    InterpretNumber(n);
                else
                    Throw($"{s} is not a recognized word or number.");
        }
    }
    internal string DotS()
    {
        StringBuilder sb = new();
        for (int i = 0; i < sp; i += CELL_SIZE)
        {
            sb.Append(ReadCell(ps, i)); sb.Append(' ');
        }
        return sb.ToString();
    }
    internal void Bl()    => Push(' ');
    void State() => Push(state);

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
    void Execute() {
        var op = cs[ip];
        ip++;
        Cell n;
        string s;
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
            case Op.Count:
                Count();
                break;
            case Op.Refill:
                Refill();
                break;
            case Op.Word:
                WordW();
                break;
            case Op.Comma:
                Comma();
                break;
            case Op.Here:
                Here();
                break;
            case Op.At:
                At();
                break;
            case Op.Store:
                Store();
                break;
            case Op.State:
                State();
                break;
            case Op.Bl:
                Bl();
                break;
            case Op.Dup:
                Dup();
                break;
            case Op.Swap:
                Swap();
                break;
            case Op.Dup2:
                Dup2();
                break;
            case Op.Drop:
                Drop();
                break;
            case Op.Drop2:
                Drop2();
                break;
            case Op.Find:
                Find();
                break;
            case Op.Colo:
                s = NextWord();
                if(string.IsNullOrWhiteSpace(s)) Throw("Colon needs a subsequent word in the stream.");
                //dict.Add(new Word { Name = s, Ip = ip, Immediate = false});
                Executing = false;
                PushOp(Op.Nest);
                // put vm in compilation mode
                // emit Nest
                break;
            case Op.Semi:
                // Emit UnNest
                break;
            default:
                Throw($"{(Op)op} bytecode not supported.");
                break;
        }
    }
    /** These are internal to be able to test them. What a bother. **/
    internal void Push(Cell n)  => ps = Utils.Add(ps, ref sp, n);
    internal Cell Pop()         => Utils.ReadBeforeIndex(ps, ref sp);
    internal Cell Peek()        => Utils.ReadCell(ps, sp - CELL_SIZE);

    internal void RPush(Cell n) => rs = Utils.Add(rs, ref rp, n);
    internal Cell RPop()        => Utils.ReadBeforeIndex(rs, ref rp);

    internal void DPush(Cell n) => ds = Utils.Add(ds, ref herep, n);

    internal void Comma()       => ds = Utils.Add(ds, ref herep, Pop());
    internal void Store()       => Utils.WriteCell(ds, (Index)Pop(), Pop());
    internal void At()          => Push(Utils.ReadCell(ds, (Index)Pop()));
    internal void Here()        => Push(herep);
    internal void Depth()       => Push(sp / CELL_SIZE);
    internal void Swap() { var a = Pop(); var b = Pop(); Push(a); Push(b); }

    /** Refill fills out the input buffer at source, translating from UTF16 (.net) to UTF8 (what I want).
     * It would be nice to have a separate segment for strings, but Forth requires them to be store in the
     * Data Store, despite that been an array of long/ints generally. That is to allow mixed data structures
     * containing both strings and values.
     **/
    internal void Refill()
    {
        if(NextLine == null) Throw("Trying to Refill, without having passed a NextLine func");

        var inputBuffer = NextLine();

        if (inputBuffer == null)
        {
            Push(FALSE);
        }
        else
        {
            input_len_chars = Encoding.UTF8.GetByteCount(inputBuffer);
            if (input_len_chars > source_max_chars)
                throw new Exception(
                $"Cannot parse a line longer than {source_max_chars}. {inputBuffer} is {input_len_chars} chars long.");
            var inputCharSpan = ToChars(source, input_len_chars);
            var bytes = new Span<byte>(Encoding.UTF8.GetBytes(inputBuffer));
            bytes.CopyTo(inputCharSpan);
            WriteCell(ds, inp, 0);
            Push(TRUE);
        }
    }
    internal void Source()
    {
        Push(source);
        Push(input_len_chars);
    }

    internal void Compare()
    {
        var u2 = (Index)Pop();
        var a2 = (Index)Pop();
        var u1 = (Index)Pop();
        var a1 = (Index)Pop();

        var s1 = new Span<AChar>(ds, a1, u1);
        var s2 = new Span<AChar>(ds, a2, u2);
        var r  = MemoryExtensions.SequenceCompareTo(s1, s2);
        Push(r < 0 ? -1 : r > 0 ? +1 : 0);
    }
    internal void Drop()  => sp -= CELL_SIZE;
    internal void Drop2() => sp -= CELL_SIZE * 2; 
    internal void Dup()   => Push(Peek());
    internal void Dup2()
    {
        var x2 = Pop();
        var x1 = Pop();
        Push(x1);
        Push(x2);
        Push(x1);
        Push(x2);
    }

    /** It is implemented like this to avoid endianess problems **/
    void CFetch()
    {
        var c = (Index)Pop();
        var sl = new Span<AUnit>(ds, c, 1);
        Push(sl[0]);
    }

    internal void Count()
    {
        var start = (Index) Pop();
        Push(start + 1);
        Push(ds[start]);
    }
    internal string ToDotNetString()
    {
        var c = (Index)Pop();
        var a = (Index)Pop();
        var s = new Span<AUnit>(ds, a, c);
        return Encoding.UTF8.GetString(s);
    }
    /** TODO: the delimiter in this implemenation (and Forth) as to be one byte char, but UTF8 puts that into question **/
    internal void WordW(bool inKeyword = false)
    {
        var delim = (byte)Pop();
        var s = ToChars(source, input_len_chars);
        var toPtr = inKeyword ? keyWord : word;

        var w = ToChars(toPtr, word_max_chars);

        var j = 1; // It is a counted string, the first 4 bytes contains the length

        ref var index = ref ds[this.inp];

        while (index < input_len_chars && s[(Index)index] == delim) { index++; }

        // If all spaces to the end of the input, return a string with length 0. 
        if (index >= input_len_chars)
        {
            w[0] = (byte)0;
            Push(toPtr);
            return;
        }

        // Copy chars until end of space allocated, end of buffer or delim.
        while (j < word_max_chars && index < input_len_chars && s[(Index)index] != delim)
        {
            var c = s[(Index)index];
            index++;
            w[j++] = c;
        }
        // Points past the delimiter. Otherwise it would stay on last " of a string.
        if(index < input_len_chars) index++;
        if (j >= word_max_chars) throw new Exception($"Word longer than {word_max_chars}: {Encoding.UTF8.GetString(s)}");

        w[0] = (byte)(j - 1);  // len goes into the first char
        Push(toPtr);
    }
    string NextWord(Code c = (byte)' ')
    {
        Push(c);
        WordW();
        Count();
        return ToDotNetString();
    }
    Span<byte> ToChars(Index start, Index lenInBytes)
            => new(ds, start, lenInBytes);
}

public class ForthException: Exception { public ForthException(string s): base(s) { } };

static class Utils {

    const AChar HighBit     = 0b10000000; 
    const AChar HighBitMask = 0b01111111;

    internal static bool  IsHighBitSet(AChar c) => (HighBit & c) != 0;
    internal static AChar ResetHighBit(AChar c) => (AChar)(HighBitMask & c);
    internal static AChar HighBitValue(AChar c) => (AChar) (c >> 7);

    internal static void WriteCell(AUnit[] ar, Index i, Cell c)
#if CELL32
        => BinaryPrimitives.WriteInt32LittleEndian(new Span<byte>(ar, i, Vm.CELL_SIZE), c);
#else
        => BinaryPrimitives.WriteInt64LittleEndian(new Span<byte>(ar, i, Vm.CELL_SIZE), c);
#endif

    internal static Cell ReadCell(AUnit[] ar, Index i)
#if CELL32
        => BinaryPrimitives.ReadInt32LittleEndian(new Span<byte>(ar, i, Vm.CELL_SIZE));
#else
        => BinaryPrimitives.ReadInt64LittleEndian(new Span<byte>(ar, i, Vm.CELL_SIZE));
#endif

    internal static AUnit[] Add(AUnit[] a, ref Index i, Cell t) {
        if(i + Vm.CELL_SIZE >= a.Length) Array.Resize(ref a, i * 2);
        WriteCell(a, i, t);        
        i += Vm.CELL_SIZE;
        return a;
    }
    internal static Cell ReadBeforeIndex(AUnit[] a, ref Index i)
    {
        i -= Vm.CELL_SIZE;
        return ReadCell(a, i);
    }
    [DoesNotReturn]
    internal static void Throw(string message) => throw new ForthException(message);

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
