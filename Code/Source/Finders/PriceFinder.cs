using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class PriceFinder<TSource, TSourcedItem> : BestFinder<TSource, TSourcedItem>, IPriceFinder<TSource, TSourcedItem>
        where TSource : ISelectable
        where TSourcedItem : IPrice, IComparable<TSourcedItem>
    {
        public PriceFinder(double amount, Dictionary<TSource, TSourcedItem> dict) : base(dict)
        {
            Amount = amount;
        }

        public double UnitPrice => HasBestPair ? BestPairs[0].Value.Price : 0;
        public double Amount { get; }
        public double Price => Amount * UnitPrice;
    }
}
