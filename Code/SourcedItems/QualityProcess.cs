namespace StardewValleyStonks
{
    public class QualityProcess : Process, IProcess
    {
        public override int InputAmount { get; }

        readonly double[] _OutputAmounts;

        public override double OutputAmount(QualityItem input) => _OutputAmounts[input.Quality];
        public override double ProfitPerInput(int quality) => Output(quality).Price * _OutputAmounts[quality] / InputAmount;
        public override double MaxOutput(QualityDist inputs)
        => inputs / InputAmount * _OutputAmounts[inputs.Quality];

        //double OutputAmount(int quality) => AmountWith[quality];
        //public double Profit(double output) => Output.Price * output;

        public QualityProcess(
           Item input,
           int inputAmount,
           Processor processor,
           Item output,
           double[] outputAmounts,
           ICondition[] conditions = null)
           : base(input, processor, output, conditions)
        {
            InputAmount = inputAmount;
            _OutputAmounts = outputAmounts;
        }
    }
}
