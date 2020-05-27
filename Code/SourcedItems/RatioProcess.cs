namespace StardewValleyStonks
{
    public class RatioProcess : Selectable, IProcess
    {
        public Item Input { get; }
        public int InputAmount { get; }
        public Processor Source { get; }
        public double OutputAmount { get; }
        public override bool Active => base.Active && Source.Active;

        readonly Item _Output;

        public IItem Output(IItem input) => Output(input.Quality);
        public double ProfitPerInput(int quality) => Output(quality).Price * OutputAmount / InputAmount;
        public int CompareTo(IProcess other, int quality) =>
            ProfitPerInput(quality).CompareTo(other.ProfitPerInput(quality));

        public double Profit(double output) => Output.Price * output;
        public double MaxOutput(QualityDist inputs)
            => inputs.AllQualities / InputAmount * OutputAmount;

        private IItem Output(int quality) =>
            Source.PreservesQuality ?
            _Output.WithQuality[quality] :
            _Output.Normal;

        public RatioProcess(
           Item input,
           int inputAmount,
           Processor processor,
           Item output,
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
