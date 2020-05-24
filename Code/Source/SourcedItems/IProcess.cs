using System.Collections.Generic;

namespace StardewValleyStonks
{
    public interface IProcess : ISelectable
    {
        public Dictionary<IItem, int> Inputs { get; }
        public Processor Source { get; }
        //future feature? instead use:
        //Dictionary<IItem, int> Outputs
        public IItem OutputItem { get; }
        public double OutputAmount { get; }

        public bool HasInput(IItem item);
        public bool SameInputs(IProcess other);
        public double MaxOutput(Dictionary<IItem, double> inputs);
        public double Profit(double output);
        public Dictionary<IItem, List<(IProcess, double)>> ConsumeInput(Dictionary<IItem, double> inputs, double output);
    }
}