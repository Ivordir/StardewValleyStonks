using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class SingleProcess : Selectable, IProcess, IComparable<SingleProcess>
    {
        public Source Source { get; }
        public IItem InputItem { get; }
        public double InputAmount { get; }
        public IItem OutputItem { get; }
        public double OutputAmount { get; }

        public SingleProcess(
            Source source,
            IItem item,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            Source = source;
            InputItem = item;
            InputAmount = 1;
            OutputItem = item;
            OutputAmount = 1;
        }

        public SingleProcess(
            Source source,
            IItem input,
            int inputAmount,
            IItem output,
            int outputAmount,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            Source = source;
            InputItem = input;
            InputAmount = inputAmount;
            OutputItem = output;
            OutputAmount = outputAmount;
        }

        public double MaxOutput(Dictionary<IItem, double> inputs)
        {
            if (!inputs.ContainsKey(OutputItem))
            {
                return 0;
            }
            return inputs[OutputItem];
        }

        public double Profit(double output)
        {
            return OutputItem.Price * output;
        }

        public Dictionary<IItem, List<(IProcess, double)>> ConsumeInput(Dictionary<IItem, double> inputs, double output)
        {
            inputs[InputItem] -= output * InputAmount;
            if (inputs[InputItem] == 0)
            {
                inputs.Remove(InputItem);
            }
            return new Dictionary<IItem, List<(IProcess, double)>>
            {
                { InputItem, new List<(IProcess, double)> { (this, output * InputAmount) } }
            };
        }

        public override bool Active => base.Active && Source.Active;

        public double ProfitPerInput => OutputAmount * OutputItem.Price / InputAmount;

        public int CompareTo(SingleProcess other)
        {
            return ProfitPerInput.CompareTo(other.ProfitPerInput);
        }
    }
}
