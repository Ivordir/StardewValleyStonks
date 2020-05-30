namespace StardewValleyStonks
{
    public class RatioProcess : Process, IProcess
    {
        public override int InputAmount { get; }

        readonly double _OutputAmount;

        public override double OutputAmount(QualityItem input) => _OutputAmount;
        public override double ProfitPerInput(int quality) => Output(quality).Price * _OutputAmount / InputAmount;
        public override double MaxOutput(QualityDist inputs)
        => inputs / InputAmount * _OutputAmount;

        //double OutputAmount(int quality) => AmountWith[quality];
        //public double Profit(double output) => Output.Price * output;
        //readonly double[] AmountWith; //[quality]

        public RatioProcess(
           Item input,
           int inputAmount,
           Processor processor,
           Item output,
           double outputAmount,
           ICondition[] conditions = null)
           : base(input, processor, output, conditions)
        {
            InputAmount = inputAmount;
            _OutputAmount = outputAmount;
        }
    }
}
