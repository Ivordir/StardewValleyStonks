using System.Collections.Generic;
using System.Linq;
using ExtentionsLibrary.Collections;

namespace StardewValleyStonks
{
    public class Process : Selectable, IProcess
    {
        public Processor Source { get; }
        public Dictionary<IItem, int> Inputs { get; }
        public IItem OutputItem { get; }
        public double OutputAmount { get; }
        public override bool Active => base.Active && Source.Active;

        public bool HasInput(IItem item) => Inputs.ContainsKey(item);
        public bool SameInputs(IProcess other) => Inputs.SameKeys(other.Inputs);
        public double Profit(double output) => OutputItem.Price * output;
        public double MaxOutput(Dictionary<IItem, double> inputs)
            => inputs.ContainsAllKeys(Inputs) ?
                OutputAmount * Inputs.Min(i => inputs[i.Key] / i.Value)
                : 0;

        public Dictionary<IItem, List<(IProcess, double)>> ConsumeInput(Dictionary<IItem, double> inputs, double output)
        {
            Dictionary<IItem, List<(IProcess, double)>> consumed = new Dictionary<IItem, List<(IProcess, double)>>();
            foreach (IItem requiredItem in Inputs.Keys)
            {
                inputs[requiredItem] -= output * Inputs[requiredItem];
                consumed.Add(requiredItem, new List<(IProcess, double)> { (this, output * Inputs[requiredItem]) });
                if (inputs[requiredItem] == 0)
                {
                    inputs.Remove(requiredItem);
                }
            }
            return consumed;
        }

        public Process(
            Processor processor,
            Dictionary<IItem, int> inputs,
            IItem outputItem,
            double outputAmount,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            Source = processor;
            Inputs = inputs;
            OutputItem = outputItem;
            OutputAmount = outputAmount;
        }
    }
}
