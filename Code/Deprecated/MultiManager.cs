using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    //finds the best TSourcedItem(s) from a one-to-many map of TSource to TSourcedItem s
    public class MultiManager<TSource, TSourcedItem> : ListManager<TSource, TSourcedItem>
        where TSource : IActiveItem
        where TSourcedItem : IActiveItem, IComparable<TSourcedItem>
    {
        public MultiManager(Dictionary<TSource, TSourcedItem[]> itemsFrom) : base(itemsFrom) { }

        protected override int Compare(TSourcedItem item)
        {
            return ItemsFrom[BestSources[0]][0].CompareTo(item);
        }
    }
}
