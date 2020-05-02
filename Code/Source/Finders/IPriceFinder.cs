using System;

namespace StardewValleyStonks
{
    public interface IPriceFinder<TSource, TSourcedItem> : IBestFinder<TSource, TSourcedItem>
        where TSource : ISelectable
        where TSourcedItem : IPrice, IComparable<TSourcedItem>
    {
        public double Amount { get; set; }
        public double UnitPrice { get; }
        public double Price { get; }
    }
}