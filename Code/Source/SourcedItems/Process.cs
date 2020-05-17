using System.Collections.Generic;
using System.Linq;

namespace StardewValleyStonks
{
    public class Process : Selectable, ISelectable
    {
        public Process(
            IProcessor processor,
            Dictionary<IItem, int> inputs,
            IItemAmount output,
            ICondition[] conditions)
            : base(true, conditions)
        {
            Processor = processor;
            Inputs = inputs;
            Output = output;
        }

        public IProcessor Processor { get; }
        public Dictionary<IItem, int> Inputs { get; }
        public IItemAmount Output { get; }

        public double Profit(Dictionary<IItem, double> inputs)
        {
            double output = MaxUnitOutput(inputs);
            foreach(IItem input in inputs.Keys)
            {
                inputs[input] -= output * Inputs[input];
                if (inputs[input] == 0)
                {
                    inputs.Remove(input);
                }
            }
            foreach(IItem input in inputs.Keys.Where(k => inputs[k] == 0))
            {
                inputs.Remove(input);
            }
            return Output.Item.Price * Output.Amount * output;
        }
        private double MaxUnitOutput(Dictionary<IItem, double> inputs)
        {
            return inputs.Min(i => i.Value / Inputs[i.Key]);
        }
        public double MaxOutput(Dictionary<IItem, double> inputs)
        {
            return Output.Amount * MaxUnitOutput(inputs);
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
        public override bool Active => base.Active && Processor.Active;
    }
}
