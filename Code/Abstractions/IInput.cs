namespace StardewValleyStonks
{
    public interface IInput
    {
        public IPriceTracker<ISource, IPricedItem> Item { get; }
        public double OutputPerItem { get; }

        public double UnitPrice { get; }
        public double Output { get; }
        public double Price { get; }
    }
}