using System.Collections.Generic;

namespace StardewValleyStonks
{
    public interface IRanker<TSource, TSourcedItem>
        where TSource : IActiveItem
        where TSourcedItem : IActiveItem
    {
        public List<List<KeyValuePair<TSource, TSourcedItem>>> BestItems { get; }
        public Dictionary<TSource, List<TSourcedItem>> ItemsFrom { get; }

        public void Add(TSource source, List<TSourcedItem> sourcedItem);
        public void Remove(TSource source);
        public void ToggleItemsFrom(TSource source);
        public void ReRank();
    }
}