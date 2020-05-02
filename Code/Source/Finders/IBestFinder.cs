using System.Collections.Generic;

namespace StardewValleyStonks
{
    public interface IBestFinder<TSource, TSourcedItem>
        where TSource : ISelectable
        where TSourcedItem : ISelectable
    {
        public TSourcedItem this[TSource source] { get; }
        public List<KeyValuePair<TSource, TSourcedItem>> BestPairs { get; }

        public bool HasBestPair { get; }
    }
}