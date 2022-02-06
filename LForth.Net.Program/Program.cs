using Forth;
using static System.Console;

Vm vm = new();
vm.NextLine = NextLine;

WriteLine("Say 'bye' to exit. No output means all good.");
System.ReadLine.HistoryEnabled = true;

while(true)
    try
    {
        vm.Quit();
    } catch(ForthException e) { 
        ColorLine(ConsoleColor.Red, e.Message);
        vm.Reset();
    } catch(ArgumentOutOfRangeException e) {
        if(e.StackTrace?.ToString().Contains("Pop") is not null)
            ColorLine(ConsoleColor.Red, "Possible stack underflow (it is expensive to check). Enable 'debug' to see the full exception.");
        if(vm.Debug) ColorLine(ConsoleColor.Gray, e.ToString());
        vm.Reset();
    } catch(Exception e)
    {
        ColorLine(ConsoleColor.Red, e.Message + " Enable 'debug' to see the full exception.");
        if(vm.Debug) ColorLine(ConsoleColor.Gray, e.ToString());
        vm.Reset();
    }

string NextLine() {
    
        if(CursorLeft != 0) WriteLine();

        System.ReadLine.AutoCompletionHandler = new AutoCompletionHandler(vm);

        CursorVisible = true;
        var line = System.ReadLine.Read();
        CursorVisible = false;

        return line;
};

void ColorLine(ConsoleColor color, string s) {
    var backupcolor = ForegroundColor;
    ForegroundColor = color;
    Console.WriteLine(s);
    ForegroundColor = backupcolor;
}

class AutoCompletionHandler : IAutoCompleteHandler
{
    public char[] Separators { get; set; } = new char[] { ' ' };
    public IEnumerable<string> words;

    public string[] GetSuggestions(string text, int index)
    {
        if(string.IsNullOrWhiteSpace(text)) return words.ToArray();

        return words.Where(
                s => s.StartsWith(
                text.Split(' ', StringSplitOptions.TrimEntries | StringSplitOptions.TrimEntries).Last())
                ).ToArray();
    }

    public AutoCompletionHandler(Vm vm) {
        words = vm.Words();
    }
}