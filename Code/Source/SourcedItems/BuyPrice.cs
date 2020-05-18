using System;

namespace StardewValleyStonks
{
    public class BuyPrice : Selectable, IComparable<BuyPrice>
    {
        public int Price { get; }
        public Source Source { get; }

        public override bool Active => base.Active && Source.Active;

        public BuyPrice(
            int price,
            bool enabled = true,
            ICondition[] conditions = null)
            : base(enabled, conditions)
        {
            Price = price;
        }

        public int CompareTo(BuyPrice other)
        {
            return -1 * Price.CompareTo(other.Price);
        }
    }
}
