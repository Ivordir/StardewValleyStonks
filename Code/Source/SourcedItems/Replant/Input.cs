namespace StardewValleyStonks
{
    public class Input : IInput
    {
        public IPriceTracker<ISource, IPrice> InputItem { get; }
        public double OutputPerInput { get; }

        public Input(IPriceTracker<ISource, IPrice> inputItem, double amount)
        {
            InputItem = inputItem;
            OutputPerInput = amount;
        }

        public double UnitPrice => InputItem.UnitPrice / OutputPerInput;
        public double Output => InputItem.Amount * OutputPerInput;
    }
}
