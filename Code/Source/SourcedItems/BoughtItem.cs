namespace StardewValleyStonks
{
    public class BoughtItem : ActiveItem, IPricedItem
    {
        public int UnitPrice { get; }

        public BoughtItem(int price, bool enabled = true, ICondition[] conditions = null) : base(enabled, conditions)
        {
            UnitPrice = price;
        }

        public int CompareTo(IPricedItem other)
        {
            return -1 * UnitPrice.CompareTo(other.UnitPrice);
        }
    }
}
