using System.Collections.Generic;
using System.Linq;

namespace StardewValleyStonks
{
    public class Process : Selectable, IProcess
    {
        public Source Source { get; }
        public Dictionary<IItem, int> Inputs { get; }
        public IItem OutputItem { get; }
        public double OutputAmount { get; }

        public override bool Active => base.Active && Source.Active;

        public double Profit(double output) => OutputItem.Price * output;

        public double MaxOutput(Dictionary<IItem, double> inputs)
        {
            foreach (IItem requiredInput in Inputs.Keys)
            {
                if (!inputs.ContainsKey(requiredInput))
                {
                    return 0;
                }
            }
            return OutputAmount * Inputs.Min(i => inputs[i.Key] / i.Value);
        }

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
            Source processor,
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
