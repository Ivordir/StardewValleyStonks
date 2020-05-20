using System.Collections.Generic;

namespace StardewValleyStonks
{
    public interface IProcess : ISelectable
    {
        public Dictionary<IItem, int> Inputs { get; }
        public IItem OutputItem { get; }
        public double OutputAmount { get; }
        public Source Source { get; }

        public double MaxOutput(Dictionary<IItem, double> inputs);
        public double Profit(double output);
        public Dictionary<IItem, List<(IProcess, double)>> ConsumeInput(Dictionary<IItem, double> inputs, double output);
    }
}