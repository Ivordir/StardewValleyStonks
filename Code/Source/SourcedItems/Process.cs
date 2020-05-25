using System.Collections.Generic;
using System.Linq;
using ExtentionsLibrary.Collections;

namespace StardewValleyStonks
{
    public class Process : Selectable, INullComparable<Process>
    {
        public Processor Source { get; }
        public Dictionary<IItem, int> Inputs { get; }
        public IItem OutputItem => Source.PreservesQuality ? _OutputItem : _OutputItem.Normal;
        public double OutputAmount { get; }
        public override bool Active => base.Active && Source.Active;

        public int? CompareTo(Process other)
        {
            bool superSet = Inputs.IsSuperSetOf(other.Inputs);
            bool subSet = Inputs.IsSubSetOf(other.Inputs);
            if (subSet && superSet) //same inputs
            {
                bool anyBetter = false;
                bool anyWorse = false;
                foreach (IItem item in Inputs.Keys)
                {
                    if (Inputs[item] > other.Inputs[item])
                    {
                        anyWorse = true;
                    }
                    else if (Inputs[item] < other.Inputs[item])
                    {
                        anyBetter = true;
                    }
                    if (anyBetter && anyWorse)
                    {
                        return null;
                    }
                }
                if (anyBetter)
                {
                    return 1;
                }
                else if (anyWorse)
                {
                    return -1;
                }
                return 0;
            }
            else if (superSet)
            {
                if (other.Inputs.Keys.Any(k => Inputs[k] < other.Inputs[k]))
                {
                    return null;
                }
                return -1;
            }
            else if (subSet)
            {
                if (Inputs.Keys.Any(k => Inputs[k] > other.Inputs[k]))
                {
                    return null;
                }
                return 1;
            }
            return null;
        }
        public bool HasInput(IItem item) => Inputs.ContainsKey(item);
        public double Profit(double output) => OutputItem.Price * output;
        public double MaxOutput(Dictionary<IItem, double> inputs)
            => OutputAmount * Inputs.Min(i => inputs[i.Key] / i.Value);
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
            IItem sell,
            ICondition[] conditions = null)
        : this(
            processor,
            sell,
            1,
            sell,
            1,
            conditions) { }
        //public Process(
        //    Processor processor,
        //    IItem input,
        //    IItem output,
        //    ICondition[] conditions = null)
        //: this(
        //    processor,
        //    input,
        //    1,
        //    output,
        //    1,
        //    conditions) { }
        //public Process(
        //    Processor processor,
        //    IItem input,
        //    int inputAmount,
        //    IItem output,
        //    ICondition[] conditions = null)
        //: this(
        //    processor,
        //    input,
        //    inputAmount,
        //    output,
        //    1,
        //    conditions) { }
        //public Process(
        //    Processor processor,
        //    IItem item,
        //    IItem output,
        //    double outputAmount,
        //    ICondition[] conditions = null)
        //: this(
        //    processor,
        //    item,
        //    1,
        //    output,
        //    outputAmount,
        //    conditions) { }
        public Process(
            Processor processor,
            IItem input,
            int inputAmount,
            IItem output,
            double outputAmount,
            ICondition[] conditions = null)
        : this(
            processor,
            new Dictionary<IItem, int>() { { input, inputAmount } },
            output,
            outputAmount,
            conditions) { }
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
