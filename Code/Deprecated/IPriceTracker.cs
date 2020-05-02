namespace StardewValleyStonks
{
    public interface IPriceTracker<TSource, TSourcedItem> : IBestItemTracker<TSource, TSourcedItem>
        where TSource : ISelectable
        where TSourcedItem : IPrice
    {
        public double Amount { get; set; }
        public double UnitPrice { get; }
        public double Price { get; }
    }
}
