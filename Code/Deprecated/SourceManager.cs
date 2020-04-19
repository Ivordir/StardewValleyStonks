using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    //finds the best TSourcedItem(s) from a one-to-one map of TSource to TSourcedItem
    public class SourceManager<TSource, TSourcedItem> : Manager<TSource, TSourcedItem>
        where TSource : IActiveItem
        where TSourcedItem : IActiveItem, IComparable<TSourcedItem>
    {
        public SourceManager(Dictionary<TSource, TSourcedItem> itemFrom) : base(itemFrom) { }

        protected override int Compare(TSource source)
        {
            return ItemFrom[BestSources[0]].CompareTo(ItemFrom[source]);
        }
    }
}
