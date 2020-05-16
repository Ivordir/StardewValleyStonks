﻿using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class BestFinder<TSource, TSourcedItem> : IBestFinder<TSource, TSourcedItem>
        where TSourcedItem : ISelectable, IComparable<TSourcedItem>
    {
        private readonly Dictionary<TSource, TSourcedItem> Dict;
        protected readonly List<TSourcedItem> _BestItems;

        public BestFinder(Dictionary<TSource, TSourcedItem> dict)
        {
            Dict = dict;
            _BestItems = new List<TSourcedItem>();
        }

        public bool HasBestItem => BestItems.Count != 0;

        public TSourcedItem this [TSource source] => Dict[source];
        
        public List<TSourcedItem> BestItems
        {
            get
            {
                _BestItems.Clear();
                foreach (TSourcedItem item in Dict.Values)
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

        public bool HasSource(TSource source) => Dict.ContainsKey(source);

        public Dictionary<TSource, TSourcedItem>.KeyCollection Sources => Dict.Keys;

        public Dictionary<TSource, TSourcedItem>.ValueCollection Items => Dict.Values;

        protected virtual int Compare(TSourcedItem item)
        {
            return _BestItems[0].CompareTo(item);
        }
    }
}