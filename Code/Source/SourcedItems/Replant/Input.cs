namespace StardewValleyStonks
{
    public class Input : IInput
    {
        public IPriceManager<ISource, IPricedItem> Item { get; }
        public double Amount { get; } //amount of output per item inputed

        public Input(IPriceManager<ISource, IPricedItem> item, double amount)
        {
            Item = item;
            Amount = amount;
        }

        public double Price => Item.UnitPrice * Amount;
        public double Output => Item.Amount * Amount;
    }
}
