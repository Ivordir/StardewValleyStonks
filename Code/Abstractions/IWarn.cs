using System.Collections.Generic;

namespace StardewValleyStonks
{
    public interface IWarn
    {
        public List<Warning> Warnings { get; }
        public string DisplayWarnings { get; }
    }
}
