namespace Forth;

using System.Diagnostics.CodeAnalysis;
using static Forth.Utils;


public enum Op {
    Error , Colo, Semi, Does, Plus, Minu, Mult, Divi, Prin,
    Count, Word, Refill, Comma, Here, At, Store, State, Bl, Dup, Exit,
    Swap, Dup2, Drop, Drop2, Find, Bye, DotS, Interpret, Quit, Create, RDepth, Depth,
    Less, More, Equal, NotEqual,// End of 1 byte
    Branch0, RelJmp, // End of 2 byte size
    NumbEx, // End of CELL Size 
    Jmp , Numb, Call, // End of Var number
    FirstHasVarNumb = Jmp, FirstHas2Size = Branch0, FirstHasCellSize = NumbEx,
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

    Index sp     = 0;
    Index rp     = 0;
    Index herep  = 0;
    AUnit[] ps;
    AUnit[] rs;
    AUnit[] ds;

    readonly Index code = 0;

    Index dictHead;

    /** There are some words that directly maps to single opcodes **/
    readonly Dictionary<string, Op> WordToSimpleOp = new()
    {
        { "."           , Op.Prin },
        { "count"       , Op.Count },
        { "refill"      , Op.Refill },
        { "interpret"   , Op.Interpret },
        { "quit"        , Op.Quit },
        { "word"        , Op.Word },
        { ","           , Op.Comma },
        { "here"        , Op.Here },
        { "@"           , Op.At },
        { "!"           , Op.Store },
        { "state"       , Op.State },
        { "bl"          , Op.Bl },
        { ":"           , Op.Colo },
        { "bye"         , Op.Bye },
        { ".s"          , Op.DotS },
        { "+"           , Op.Plus },
        { "-"           , Op.Minu },
        { "*"           , Op.Mult },
        { "/"           , Op.Divi },
        { "<"           , Op.Less },
        { ">"           , Op.More },
        { "="           , Op.Equal },
        { "<>"          , Op.NotEqual },
        { "create"      , Op.Create },
        { "does>"       , Op.Does },
        { "rdepth"      , Op.RDepth },
        { "depth"       , Op.Depth },
        { "dup"         , Op.Dup },
        { "dup2"        , Op.Dup2 },
        { "drop"        , Op.Drop },
        { "drop2"       , Op.Drop2 },
        { "exit"        , Op.Exit },
    };

    /** While other words need to perfom more complicated actions at compile time **/
    readonly Dictionary<string, Action> WordToDef = new();

    public Func<string>? NextLine = null;

    public Vm(
        Index maxParameterStack = Config.SmallStack,
        Index maxReturnStack    = Config.SmallStack,
        Index maxDataStack      = Config.SmallStack,
        Index maxStrings        = 128,
        Index maxPad            = 1_024,
        Index maxSource         = 1_024,
        Index maxWord           = 128
        ) {

        ps   = new AUnit[maxParameterStack];
        rs   = new AUnit[maxReturnStack];
        ds   = new AUnit[maxDataStack];

        code = herep;
        herep += CHAR_SIZE + CELL_SIZE;

        keyWord = herep;
        herep += maxWord * CHAR_SIZE;
        source  = herep;
        herep += maxSource * CHAR_SIZE;

        word    = herep;
        herep += maxWord * CHAR_SIZE;

        pad              = herep;
        herep           += maxPad;
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

        void Mark()          => Push(herep);
        void BranchAndMark() { PushOp(Op.Branch0); Mark(); herep += 2;}
        void EmbedInPoppedJmpFwd() {
                PushOp(Op.RelJmp);
                var mark = (Index)Pop();
                short delta = (short)((herep + 2) - mark);
                WriteInt16(ds, mark, delta);
                Push(herep);
                herep += 2;
        }
        void EmbedHereJmpBck() {
                PushOp(Op.RelJmp);
                var mark = (Index)Pop();
                var delta = (short)(mark - herep);
                WriteInt16(ds, herep, delta); 
                herep += 2;
        }
        WordToDef = new()
        {
            { "debug",  () => Debug = !Debug },
            { ";",      () => { PushOp(Op.Exit);  Executing = true; } },
            { "begin",        Mark },
            { "again",        EmbedHereJmpBck  },    
            { "if",           BranchAndMark },
            { "else",         EmbedInPoppedJmpFwd  },    
            { "then",   () => {
                var mark = (Index)Pop();
                short delta = (short)(herep - mark);
                WriteInt16(ds, mark, delta);
            } }
        };
    }
    public void Reset()
    {
        sp = 0; rp = 0; Executing = true; ds[inp] = 0;
    }
    public void Quit()
    {
        rp = 0; Executing = true; ds[inp] = 0; // Don't reset the parameter stack as for ANS FORTH definition of QUIT.

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
    void PushOp(Op op) {
        ds[herep] = (Code)op;
        herep++;
    }
    Index PushOp(Op op, Cell value)
    {
        PushOp(op);
        Write7BitEncodedCell(ds, herep, value, out var howMany);
        herep += howMany;
        return howMany;
    }
    Index PushExecOp(Op op, Cell? value)
    {
        ds[code] = (Code)op;
        var howMany = 0;
        if(value is not null) Write7BitEncodedCell(ds, code + 1, (Cell)value, out howMany);
        return howMany + 1;
    }
    void PushUntilExit(ref Index ip)
    {
        while(true)
        {
            var op = ds[ip];
            var count = 0;
            if(Utils.HasCellSize(op))
                count = CELL_SIZE;
            else if(Utils.HasVarNumberSize(op))
                Read7BitEncodedCell(ds, ip + 1, out count);

            Array.Copy(ds, ip, ds, herep, 1 + count);
            ip += 1 + count;
            herep += 1 + count;
            if((Op)op == Op.Exit)
                break;
        }
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
    Index Lastxt() => LinkToCode(dictHead, ds[dictHead + CELL_SIZE]);

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
    bool InterpretWord()
    {
        // TODO: optimize away string creation, requires not storing word to opcode/definition data in hashtables (I think).
        Dup();
        var sl = ToDotNetStringC();

        Find();
        var res   = Pop();
        var xt    = (Index)Pop();

        // Manage user defined word.
        if(res != 0)
        {
            var immediate = res == 1;
            if(Executing || immediate)
                Execute(Op.Call, xt);
            else
                PushOp(Op.Call, xt);
            return true;
        }
        // Manage simple primitives.
        if(WordToSimpleOp.TryGetValue(sl, out var op))
        {
            if(Executing)
                Execute(op, null);
            else
                PushOp(op);
            return true;
        }
        // Manage complex primitives.
        if(WordToDef.TryGetValue(sl, out var def))
        {
            def();
            return true;
        }
        return false;
    }
    void InterpretNumber(Cell value)
    {
        if(Executing)
            Execute(Op.Numb, value);
        else
            PushOp(Op.Numb, value);
    }
    internal bool IsEmptyWordC() => ds[Peek()] == 0;

    void Interpret()
    {
        while(true)
        {
            Bl();
            WordW();
            if(IsEmptyWordC()) { Drop(); break;};

            // TODO: remove string allocation from main loop.
            Dup();
            var s = ToDotNetStringC();
            if(!InterpretWord()) {
                if(Cell.TryParse(s, out Cell n))
                    InterpretNumber(n);
                else
                    Throw($"{s} is not a recognized word or number.");
            }
        }
    }
    internal string DotS()
    {
        StringBuilder sb = new();
        sb.Append($"<{sp / CELL_SIZE}> ");
        for (int i = 0; i < sp; i += CELL_SIZE)
        {
            sb.Append(ReadCell(ps, i)); sb.Append(' ');
        }
        return sb.ToString();
    }
    internal void Bl()    => Push(' ');
    void State() => Push(state);

    void Execute(Op op, Cell? data) {

        Index opLen = PushExecOp(op, data);
        var ip      = code;

        do {
            var currentOp = (Op)ds[ip];
            ip++;
            Cell n, flag;
            Index count, idx;
            switch(currentOp) {
                case Op.Numb:
                    n = Read7BitEncodedCell(ds, ip, out count);
                    Push(n);
                    ip += count;
                    break;
                case Op.NumbEx:
                    n = ReadCell(ds, ip);
                    Push(n);
                    ip = (Index)RPop();
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
                case Op.Drop:
                    Drop();
                    break;
                case Op.Drop2:
                    Drop2();
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
                case Op.Find:
                    Find();
                    break;
                case Op.Bye:
                    Environment.Exit(0);
                    break;
                case Op.DotS:
                    Console.WriteLine(DotS());
                    break;
                case Op.Quit:
                    Quit();
                    break;
                case Op.Interpret:
                    Interpret();
                    break;
                case Op.Plus:
                    Push(Pop() + Pop());
                    break;
                case Op.Minu:
                    Push(- Pop() + Pop());
                    break;
                case Op.Mult:
                    Push(Pop() * Pop());
                    break;
                case Op.Less:
                    Push(Pop() > Pop() ? TRUE : FALSE);
                    break;
                case Op.More:
                    Push(Pop() < Pop() ? TRUE : FALSE);
                    break;
                case Op.Equal:
                    Push(Pop() == Pop() ? TRUE : FALSE);
                    break;
                case Op.NotEqual:
                    Push(Pop() != Pop() ? TRUE : FALSE);
                    break;
                case Op.Depth:
                    Push(sp / CELL_SIZE);
                    break;
                case Op.RDepth:
                    Push(rp / CELL_SIZE);
                    break;
                case Op.Divi:
                    var d = Pop();
                    var u = Pop();
                    Push(u / d);
                    break;
                case Op.Create:
                    Push(' ');
                    WordW();
                    if(ds[Peek()] == 0) Throw("Make needs a subsequent word in the stream.");
                    DictAdd();

                    PushOp(Op.NumbEx);
                    // Need to use full cell because it gets substituted by a jmp in does>
                    // and I don't know how many cells the number I need to jump to takes
                    WriteCell(ds, herep, herep + CELL_SIZE);
                    herep += CELL_SIZE;
                    break;
                case Op.Does:
                    // Allows redefinition of last defined word!!
                    // Even in gforth!!
                    idx = Lastxt();
                    var addrToPush = ReadCell(ds, idx + 1);
                    
                    var tmpHerep = herep;
                    herep = idx;
                    PushOp(Op.Jmp, tmpHerep);
                    herep = tmpHerep;
                    PushOp(Op.Numb, addrToPush);
                    PushUntilExit(ref ip);
                    ip = (Index)RPop();
                    break;
                case Op.Colo:
                    Push(' ');
                    WordW();
                    if(ds[Peek()] == 0) Throw("Colon needs a subsequent word in the stream.");
                    DictAdd();
                    Executing = false;
                    break;
                case Op.Call:
                    n = Read7BitEncodedCell(ds, ip, out count);
                    ip += count;
                    RPush(ip);
                    ip = (Index)n;
                    break;
                case Op.Jmp:
                    n = Read7BitEncodedCell(ds, ip, out _);
                    ip = (Index) n;
                    break;
                case Op.Exit:
                    ip = (Index)RPop();
                    break;
                case Op.Branch0:
                    flag = Pop();
                    ip += flag == FALSE ? ReadInt16(ds, ip) : 2 ;
                    break;
                case Op.RelJmp:
                    ip += ReadInt16(ds, ip);
                    break;
                default:
                    Throw($"{(Op)op} bytecode not supported.");
                    break;
            }
        } while (ip != code + opLen);
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
    internal void Swap()        { var a = Pop(); var b = Pop(); Push(a); Push(b); }

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
            inputBuffer = inputBuffer.Trim();
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
    internal string ToDotNetStringC()
    {
        var a = (Index)Pop();
        var s = new Span<AChar>(ds, a + CHAR_SIZE, ds[a]);
        return Encoding.UTF8.GetString(s);
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

    internal static void WriteInt16(AUnit[] ar, Index i, Int16 c)
        => BinaryPrimitives.WriteInt16LittleEndian(new Span<byte>(ar, i, 2), c);
    internal static Int16 ReadInt16(AUnit[] ar, Index i)
        => BinaryPrimitives.ReadInt16LittleEndian(new Span<byte>(ar, i, 2));

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

    internal static bool Has2NumberSize(byte b)
        => b >= (int)Op.FirstHas2Size && b < (int)Op.FirstHasCellSize;
    internal static bool HasCellSize(byte b)
        => b >= (int)Op.FirstHasCellSize && b < (int)Op.FirstHasVarNumb;
    internal static bool HasVarNumberSize(byte b)
        => b >= (int)Op.FirstHasVarNumb;
}
