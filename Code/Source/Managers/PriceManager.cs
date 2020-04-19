using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class PriceManager<TSource, TSourcedItem> : Manager<TSource, IPricedItem>, IPriceManager<TSource, TSourcedItem>
        where TSource : ISource
        where TSourcedItem : IPricedItem
    {
        public double Amount { get; set; }

        public PriceManager(
            double amount,
            Dictionary<TSource, IPricedItem> priceFrom)
            : base(priceFrom)
        {
            Amount = amount;
        }

        public int UnitPrice => NoSource ? -1 : ItemFrom[BestSources[0]].Price;

        public double Price => Amount * UnitPrice;
    }
}
