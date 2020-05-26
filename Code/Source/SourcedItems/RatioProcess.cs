namespace StardewValleyStonks
{
    public class RatioProcess : Selectable, IProcess
    {
        public IItem Input { get; }
        public int InputAmount { get; }
        public Processor Source { get; }
        public IItem Output => Source.PreservesQuality ? _Output : _Output.Normal;
        public double OutputAmount { get; }
        public double ProfitPerInput => Output.Price * OutputAmount / InputAmount;
        public override bool Active => base.Active && Source.Active;

        public int CompareTo(IProcess other)
            => ProfitPerInput.CompareTo(other.ProfitPerInput);
        //public bool HasInput(IItem item) => Inputs.ContainsKey(item);
        public double Profit(double output) => Output.Price * OutputAmount * output;
        //public bool HasOutput(Dictionary<IItem, List<double>> inputs)
        //    => Inputs.IsSubSetOf(inputs);
        public double MaxOutput(QualityDist inputs)
            => inputs.AllQualities / InputAmount * OutputAmount;

        private readonly IItem _Output;

        public RatioProcess(
           IItem input,
           int inputAmount,
           Processor processor,
           IItem output,
           double outputAmount,
           ICondition[] conditions = null)
           : base(true, conditions)
        {
            Input = input;
            InputAmount = inputAmount;
            Source = processor;
            _Output = output;
            OutputAmount = outputAmount;
        }
    }
}
