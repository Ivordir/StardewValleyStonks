using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class SingleProcess : Selectable, IProcess, IComparable<SingleProcess>
    {
        public Source Source { get; }
        public Dictionary<IItem, int> Inputs { get; }
        public IItem OutputItem { get; }
        public double OutputAmount { get; }
        public double ProfitPerInput => OutputAmount * OutputItem.Price / InputAmount;
        public override bool Active => base.Active && Source.Active;

        public double MaxOutput(Dictionary<IItem, double> inputs) => 
            inputs.ContainsKey(OutputItem) ? inputs[OutputItem] : 0;

        public double Profit(double output) => OutputItem.Price * output;

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

        public int CompareTo(SingleProcess other) =>
            ProfitPerInput.CompareTo(other.ProfitPerInput);

        private readonly IItem InputItem;
        private readonly int InputAmount;

        public SingleProcess(
            Source source,
            IItem item,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            Source = source;
            InputItem = item;
            InputAmount = 1;
            Inputs = new Dictionary<IItem, int> { { InputItem, InputAmount } };
            OutputItem = item;
            OutputAmount = 1;
        }

        public SingleProcess(
            Source source,
            IItem input,
            int inputAmount,
            IItem output,
            double outputAmount,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            Source = source;
            InputItem = input;
            InputAmount = inputAmount;
            Inputs = new Dictionary<IItem, int> { { InputItem, InputAmount } };
            OutputItem = output;
            OutputAmount = outputAmount;
        }
    }
}
