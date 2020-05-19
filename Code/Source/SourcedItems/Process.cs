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

        public double Profit(double output)
        {
            return OutputItem.Price * output;
        }

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
            foreach (IItem requiredInput in Inputs.Keys)
            {
                inputs[requiredInput] -= output * Inputs[requiredInput];
                consumed.Add(requiredInput, new List<(IProcess, double)> { (this, output * Inputs[requiredInput]) });
                if (inputs[requiredInput] == 0)
                {
                    inputs.Remove(requiredInput);
                }
            }
            return consumed;
        }

        public override bool Active => base.Active && Source.Active;
    }
}
