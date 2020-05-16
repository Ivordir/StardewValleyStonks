using System.Collections.Generic;

namespace StardewValleyStonks
{
    public interface IBestFinder<TSource, TSourcedItem>
        where TSourcedItem : ISelectable
    {
        public TSourcedItem this[TSource source] { get; }
        public List<TSourcedItem> BestItems { get; }

        public bool HasBestItem { get; }
    }
}