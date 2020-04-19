namespace StardewValleyStonks
{
    public interface IInput
    {
        public IPriceManager<ISource, IPricedItem> Item { get; }
        public double Amount { get; }

        public double Price { get; }
        public double Output { get; }
    }
}