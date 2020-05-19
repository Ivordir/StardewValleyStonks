using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class BestList<T>
        where T : ISelectable, IComparable<T>
    {
        public List<T> BestItems
        {
            get
            {
                _BestItems.Clear();
                foreach (T item in Items)
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
        public T this[int index] => Items[index];

        private readonly List<T> _BestItems;
        private readonly List<T> Items;

        public BestList(List<T> items)
        {
            Items = items;
            _BestItems = new List<T>();
        }

        private int Compare(T item)
        {
            return _BestItems[0].CompareTo(item);
        }
    }
}
