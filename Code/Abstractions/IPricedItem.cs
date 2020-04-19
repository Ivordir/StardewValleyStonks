using System;

namespace StardewValleyStonks
{
    public interface IPricedItem : IActiveItem, IComparable<IPricedItem>
    {
        public int Price { get; }
    }
}