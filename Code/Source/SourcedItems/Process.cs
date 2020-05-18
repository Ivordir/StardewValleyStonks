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

        public double Profit(Dictionary<IItem, double> inputs)
        {
            double output = MaxUnitOutput(inputs);
            foreach (IItem input in inputs.Keys)
            {
                inputs[input] -= output * Inputs[input];
                if (inputs[input] == 0)
                {
                    inputs.Remove(input);
                }
            }
            foreach (IItem input in inputs.Keys.Where(k => inputs[k] == 0))
            {
                inputs.Remove(input);
            }
            return OutputItem.Price * OutputAmount * output;
        }
        private double MaxUnitOutput(Dictionary<IItem, double> inputs)
        {
            return inputs.Min(i => i.Value / Inputs[i.Key]);
        }
        public double MaxOutput(Dictionary<IItem, double> inputs)
        {
            return OutputAmount * MaxUnitOutput(inputs);
        }
        /*
        public bool SameInputs(Process other)
        {
            if (Inputs.Count != other.Inputs.Count)
            {
                return false;
            }
            foreach (Product input in Inputs.Keys)
            {
                if (!other.Inputs.ContainsKey(input))
                {
                    return false;
                }
            }
            return true;
        }
        */
        public override bool Active => base.Active && Source.Active;
    }
}
