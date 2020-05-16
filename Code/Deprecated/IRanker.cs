using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    public interface IRanker<TSource, TSourcedItem>
        where TSource : ISelectable
        where TSourcedItem : ISelectable, IComparable<TSourcedItem>
    {
        List<List<KeyValuePair<TSource, TSourcedItem>>> Ranks { get; }
    }
}