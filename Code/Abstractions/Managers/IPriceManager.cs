using System;

namespace StardewValleyStonks
{
    public interface IPriceManager<TSource, TSourcedItem> : IManager<TSource, IPricedItem>
        where TSource : IActiveItem
        where TSourcedItem : IPricedItem
    {
        public double Amount { get; set; }
        public int UnitPrice { get; }
        public double Price { get; }
    }
}
