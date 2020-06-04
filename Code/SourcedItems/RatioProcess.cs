namespace StardewValleyStonks
{
    public class RatioProcess : Process, IProcess
    {
        public override int InputAmount { get; }

        readonly double _OutputAmount;

        public override double OutputAmount(int quality) => _OutputAmount / InputAmount;
        public override double Profit(int quality) => Output(quality).Price * _OutputAmount / InputAmount;

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
