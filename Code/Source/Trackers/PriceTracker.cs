using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class PriceTracker<TSource, TSourcedItem> : BestItemTracker<TSource, TSourcedItem>, IPriceTracker<TSource, TSourcedItem>
        where TSource : ISource
        where TSourcedItem : IPricedItem, IComparable<TSourcedItem>
    {
        public double Amount { get; set; }

        public PriceTracker(
            double amount,
            Dictionary<TSource, TSourcedItem> priceFrom)
            : base(priceFrom)
        {
            Amount = amount;
        }

        public virtual double UnitPrice => NoSource ? 0 : ItemFrom[BestSources[0]].UnitPrice;

        public double Price => Amount * UnitPrice;

        protected override int Compare(TSource source)
        {
            return ItemFrom[BestSources[0]].CompareTo(ItemFrom[source]);
        }
    }
}
