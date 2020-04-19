using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class BoughtItem : ActiveItem, IPricedItem
    {
        public int Price { get; }

        public BoughtItem(int price, bool enabled = true, List<ICondition> conditions = null) : base(enabled, conditions)
        {
            Price = price;
        }

        public int CompareTo(IPricedItem other)
        {
            return -1 * Price.CompareTo(other.Price);
        }
    }
}
