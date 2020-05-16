using System.Collections.Generic;
using System.Linq;

namespace StardewValleyStonks
{
    public class Process : Selectable, ISelectable
    {
        public Process(
            IProcessor processor,
            Dictionary<IProduct, int> inputs,
            IItemAmount[] outputs,
            ICondition[] conditions)
            : base(true, conditions)
        {
            Processor = processor;
            Inputs = inputs;
            Outputs = outputs;
        }

        public IProcessor Processor { get; }
        public Dictionary<IProduct, int> Inputs { get; }
        public IItemAmount[] Outputs { get; }

        public double Profit(List<ItemAmount> inputs)
        {
            double maxOutput = inputs.Min(i => i.Amount / Inputs[i.Item]);
            for (int i = 0; i < inputs.Count; i++)
            {
                inputs[i].Amount -= maxOutput * Inputs[inputs[i].Item];
                if (inputs[i].Amount == 0)
                {
                    inputs.RemoveAt(i);
                    i--;
                }
            }
            return Outputs.Sum(o => o.Item.Price * o.Amount * maxOutput);
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
