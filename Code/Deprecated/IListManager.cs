using System.Collections.Generic;

namespace StardewValleyStonks
{
    public interface IListManager<TSource, TSourcedItem>
        where TSource : IActiveItem
        where TSourcedItem : IActiveItem
    {
        List<TSource> BestSources { get; }
        Dictionary<TSource, TSourcedItem[]> ItemsFrom { get; }

        void Add(TSource source, TSourcedItem[] sourcedItem);
        void Remove(TSource source);
        void ToggleItemsFrom(TSource source);
        void UpdateBestSource();
    }
}