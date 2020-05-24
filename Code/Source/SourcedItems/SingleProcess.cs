using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class SingleProcess : Selectable, IProcess, IComparable<SingleProcess>
    {
        public Processor Source { get; }
        public Dictionary<IItem, int> InputAmounts { get; }
        public IItem OutputItem => Source.PreservesQuality ? _OutputItem : _OutputItem.Normal;
        public double OutputAmount { get; }
        public double ProfitPerInput => OutputAmount * OutputItem.Price / InputAmount;
        public override bool Active => base.Active && Source.Active;

        public double MaxOutput(Dictionary<IItem, double> inputs) => inputs.ContainsKey(InputItem) ? inputs[InputItem] : 0;

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

        public int CompareTo(SingleProcess other) => ProfitPerInput.CompareTo(other.ProfitPerInput);

        private readonly IItem InputItem, _OutputItem;
        private readonly int InputAmount;

        public SingleProcess(
            IItem input,
            int inputAmount,
            Processor source,
            IItem output,
            double outputAmount,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            Source = source;
            InputItem = input;
            InputAmount = inputAmount;
            InputAmounts = new Dictionary<IItem, int> { { InputItem, InputAmount } };
            _OutputItem = output;
            OutputAmount = outputAmount;
        }

        public SingleProcess(
            IItem item,
            Processor source,
            ICondition[] conditions = null)
            : this(item, 1, source, item, 1, conditions) { }

        public SingleProcess(
            IItem input,
            Processor source,
            IItem ouput,
            ICondition[] conditions = null)
            : this(input, 1, source, ouput, 1, conditions) { }
    }
}
