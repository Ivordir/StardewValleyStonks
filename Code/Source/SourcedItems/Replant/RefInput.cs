namespace StardewValleyStonks
{
    public class RefInput : IInput
    {
        public IPriceTracker<ISource, IPricedItem> Item { get; }
        public double OutputPerItem => Amount.Ref;

        private readonly Reference<double> Amount;

        public RefInput(IPriceTracker<ISource, IPricedItem> item, Reference<double> amount)
        {
            Item = item;
            Amount = amount;
        }

        public double UnitPrice => Item.UnitPrice / Amount.Ref;
        public double Output => Item.Amount * Amount.Ref;

        public double Price => Item.Amount * Item.UnitPrice;
    }
}
