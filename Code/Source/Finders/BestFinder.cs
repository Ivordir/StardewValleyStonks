using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class BestFinder<TSource, TSourcedItem> : IBestFinder<TSource, TSourcedItem>
        where TSource : ISelectable
        where TSourcedItem : ISelectable, IComparable<TSourcedItem>
    {
        private readonly Dictionary<TSource, TSourcedItem> Dict;
        protected readonly List<KeyValuePair<TSource, TSourcedItem>> _BestPairs;

        public BestFinder(Dictionary<TSource, TSourcedItem> dict)
        {
            Dict = dict;
            _BestPairs = new List<KeyValuePair<TSource, TSourcedItem>>();
        }

        public bool HasBestPair => BestPairs.Count != 0;

        public TSourcedItem this [TSource source] => Dict[source];
        
        public List<KeyValuePair<TSource, TSourcedItem>> BestPairs
        {
            get
            {
                _BestPairs.Clear();
                foreach (KeyValuePair<TSource, TSourcedItem> kvp in Dict)
                {
                    if (kvp.Key.Active && kvp.Value.Active)
                    {
                        if (_BestPairs.Count == 0 || Compare(kvp.Value) == 0)
                        {
                            _BestPairs.Add(kvp);
                        }
                        else if (Compare(kvp.Value) == -1) //source is better than BestSource
                        {
                            _BestPairs.Clear();
                            _BestPairs.Add(kvp);
                        }
                    }
                }
                return _BestPairs;
            }
        }

        public bool HasSource(TSource source) => Dict.ContainsKey(source);

        public Dictionary<TSource, TSourcedItem>.KeyCollection Sources => Dict.Keys;

        public Dictionary<TSource, TSourcedItem>.ValueCollection Items => Dict.Values;

        protected virtual int Compare(TSourcedItem item)
        {
            return _BestPairs[0].Value.CompareTo(item);
        }
    }
}
