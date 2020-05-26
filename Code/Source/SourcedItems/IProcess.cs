using System;

namespace StardewValleyStonks
{
    public interface IProcess : ISelectable, IComparable<IProcess>
    {
        public IItem Input { get; }
        public int InputAmount { get; }
        public Processor Source { get; }
        public IItem Output { get; }
        public double OutputAmount { get; }
        public double ProfitPerInput { get; }
    }
}
