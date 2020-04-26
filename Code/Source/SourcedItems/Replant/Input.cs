namespace StardewValleyStonks
{
    public class Input : IInput
    {
        public IPriceTracker<ISource, IPricedItem> Item { get; }
        public double OutputPerItem { get; }

        public Input(IPriceTracker<ISource, IPricedItem> item, double amount)
        {
            Item = item;
            OutputPerItem = amount;
        }

        public double UnitPrice => Item.UnitPrice / OutputPerItem;
        public double Output => Item.Amount * OutputPerItem;

        public double Price => Item.Amount * Item.UnitPrice;
    }
}
