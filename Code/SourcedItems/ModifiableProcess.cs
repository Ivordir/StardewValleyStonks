namespace StardewValleyStonks
{
    public class ModifiableProcess : Process, IProcess
    {
        public override int InputAmount { get; }

        readonly IValue[] _OutputAmounts;

        public override double OutputAmount(IItem input) => _OutputAmounts[input.Quality].Value;
        public override double ProfitPerInput(int quality) => Output(quality).Price * _OutputAmounts[quality].Value / InputAmount;
        public override double MaxOutput(QualityDist inputs)
        => inputs / InputAmount * _OutputAmounts[inputs.Quality].Value;

        //double OutputAmount(int quality) => AmountWith[quality];
        //public double Profit(double output) => Output.Price * output;

        public ModifiableProcess(
           Item input,
           int inputAmount,
           Processor processor,
           Item output,
           IValue[] outputAmounts,
           ICondition[] conditions = null)
           : base(input, processor, output, conditions)
        {
            InputAmount = inputAmount;
            _OutputAmounts = outputAmounts;
        }
    }
}
