using System.Collections.Generic;
using ExtentionsLibrary.Text;

namespace StardewValleyStonks
{
    public readonly struct Warning
    {
        public readonly string Message { get; }
        public readonly List<Warning> SubWarnings { get; }
        public string Display(int indentation = 1)
        {
            // string builder? nah
            string display = Message;
            foreach (Warning warning in SubWarnings)
            {
                display += "\n" + "  ".Repeat(indentation) + "•" + warning.Display(indentation + 1);
            }
            return display;
        }

        public Warning(string message)
        {
            Message = message;
            SubWarnings = new List<Warning>();
        }
    }
}
