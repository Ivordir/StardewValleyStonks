namespace StardewValleyStonks
{
    public class RefInput : IInput
    {
        public IPriceManager<ISource, IPricedItem> Item { get; }
        public double Amount => _Amount.Ref;

        private readonly Reference<double> _Amount;

        public RefInput(IPriceManager<ISource, IPricedItem> item, Reference<double> amount)
        {
            Item = item;
            _Amount = amount;
        }

        public double Price => Item.UnitPrice * _Amount.Ref;
        public double Output => Item.Amount / _Amount.Ref;
    }
}
