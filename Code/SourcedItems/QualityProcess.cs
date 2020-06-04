namespace StardewValleyStonks
{
    public class QualityProcess : Process, IProcess
    {
        public override int InputAmount { get; }

        readonly double[] _OutputAmounts;

        public override double OutputAmount(int quality) => _OutputAmounts[quality] / InputAmount;
        public override double Profit(int quality) => Output(quality).Price * _OutputAmounts[quality] / InputAmount;

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
