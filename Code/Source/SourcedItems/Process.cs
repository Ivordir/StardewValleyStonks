﻿namespace StardewValleyStonks
{
    public class Process : Selectable, IProcess
    {
        public IItem Input { get; }
        public int InputAmount => 1;
        public Processor Source { get; }
        public IItem Output { get; }
        public double OutputAmount => 1;
        public double ProfitPerInput => Output.Price;
        public override bool Active => base.Active && Source.Active;

        public int CompareTo(IProcess other)
            => ProfitPerInput.CompareTo(other.ProfitPerInput);

        //public bool HasInput(IItem item) => Inputs.ContainsKey(item);
        public double Profit(double output) => Output.Price * output;
        //public bool HasOutput(Dictionary<IItem, List<double>> inputs)
        //    => Inputs.IsSubSetOf(inputs);
        public double MaxOutput(QualityDist inputs)
            => inputs.AllQualities / InputAmount * OutputAmount;

        private readonly IItem _Output;

        public Process(
            IItem item,
            Processor sellSource,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            Input = item;
            Source = sellSource;
            _Output = item;
        }
        public Process(
            IItem input,
            Processor processor,
            IItem output,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            Input = input;
            Source = processor;
            _Output = output;
        }
    }
}
