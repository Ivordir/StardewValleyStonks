namespace StardewValleyStonks
{
    public class RefInput : IInput
    {
        public IPriceTracker<ISource, IPrice> InputItem { get; }
        public double OutputPerInput => Amount.Value;

        private readonly Reference<double> Amount;

        public RefInput(IPriceTracker<ISource, IPrice> inputItem, Reference<double> amount)
        {
            InputItem = inputItem;
            Amount = amount;
        }

        public double UnitPrice => InputItem.UnitPrice / Amount.Value;
        public double Output => InputItem.Amount * Amount.Value;
    }
}
