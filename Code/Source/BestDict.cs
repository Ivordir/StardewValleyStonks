using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class BestDict<TSource, TItem>
        where TItem : ISelectable, IComparable<TItem>
    {
        public List<TItem> BestItems
        {
            get
            {
                _BestItems.Clear();
                foreach (TItem item in Dict.Values)
                {
                    if (item.Active)
                    {
                        if (_BestItems.Count == 0 || Compare(item) == 0)
                        {
                            _BestItems.Add(item);
                        }
                        else if (Compare(item) == -1) //source is better than BestSource
                        {
                            _BestItems.Clear();
                            _BestItems.Add(item);
                        }
                    }
                }
                return _BestItems;
            }
        }
        public bool HasBestItem => BestItems.Count > 0;
        public TItem this[TSource source] => Dict[source];
        public bool ContainsKey(TSource source) => Dict.ContainsKey(source);
        public Dictionary<TSource, TItem>.KeyCollection Keys => Dict.Keys;
        public Dictionary<TSource, TItem>.ValueCollection Values => Dict.Values;

        private readonly Dictionary<TSource, TItem> Dict;
        private readonly List<TItem> _BestItems;

        private int Compare(TItem item)
        {
            return _BestItems[0].CompareTo(item);
        }

        public BestDict(Dictionary<TSource, TItem> dict)
        {
            Dict = dict;
            _BestItems = new List<TItem>();
        }
    }
}
