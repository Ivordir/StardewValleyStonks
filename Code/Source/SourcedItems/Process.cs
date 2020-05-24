using System.Collections.Generic;
using System.Linq;
using ExtentionsLibrary.Collections;

namespace StardewValleyStonks
{
    public class Process : Selectable, ICanCompare<Process>
    {
        public Processor Source { get; }
        public Dictionary<IItem, int> Inputs { get; }
        public IItem OutputItem => Source.PreservesQuality ? _OutputItem : _OutputItem.Normal;
        public double OutputAmount { get; }
        public override bool Active => base.Active && Source.Active;

        public bool CanCompareTo(Process other)
        {
            IItem item = Inputs.Keys.First();
            bool comparison = Inputs[item] >= other.Inputs[item];
            return Inputs.Keys.All(k => (Inputs[k] >= other.Inputs[k]) == comparison);
        }
        public int CompareTo(Process other)
        {
            IItem item = Inputs.Keys.First();
            return Inputs[item].CompareTo(other.Inputs[item]);
        }
        public bool HasInput(IItem item) => Inputs.ContainsKey(item);
        public bool SameInputs(Process other) => Inputs.SameKeys(other.Inputs);
        public double Profit(double output) => OutputItem.Price * output;
        public double MaxOutput(Dictionary<IItem, double> inputs)
            => inputs.ContainsAllKeys(Inputs) ?
                OutputAmount * Inputs.Min(i => inputs[i.Key] / i.Value)
                : 0;

        public Dictionary<IItem, List<(Process, double)>> ConsumeInput(Dictionary<IItem, double> inputs, double output)
        {
            Dictionary<IItem, List<(Process, double)>> consumed = new Dictionary<IItem, List<(Process, double)>>();
            foreach (IItem requiredItem in Inputs.Keys)
            {
                inputs[requiredItem] -= output * Inputs[requiredItem];
                consumed.Add(requiredItem, new List<(Process, double)> { (this, output * Inputs[requiredItem]) });
                if (inputs[requiredItem] == 0)
                {
                    inputs.Remove(requiredItem);
                }
            }
            return consumed;
        }

        private readonly IItem _OutputItem;

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
            _OutputItem = outputItem;
            OutputAmount = outputAmount;
        }
    }
}
