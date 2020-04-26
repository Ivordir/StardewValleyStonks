namespace StardewValleyStonks
{
    public interface IPriceTracker<TSource, TSourcedItem> : IBestItemTracker<TSource, TSourcedItem>
        where TSource : IActiveItem
        where TSourcedItem : IPricedItem
    {
        public double Amount { get; set; }
        public double UnitPrice { get; }
        public double Price { get; }
    }
}
