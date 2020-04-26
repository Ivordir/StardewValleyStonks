using System.Collections.Generic;

namespace StardewValleyStonks
{
    public interface IBestItemTracker<TSource, TSourcedItem>
        where TSource : IActiveItem
        where TSourcedItem : IActiveItem
    {
        public List<TSource> BestSources { get; }
        public Dictionary<TSource, TSourcedItem> ItemFrom { get; }

        public bool NoSource { get; }
        public bool HasSource { get; }
        public void Add(TSource source, TSourcedItem sourcedItem);
        public void Remove(TSource source);
        public void ToggleItemFrom(TSource source);
        public void UpdateBestSource();
    }
}