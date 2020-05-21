using System;

namespace StardewValleyStonks
{
    public class BuyPrice : Selectable, IComparable<BuyPrice>
    {
        public int Price { get; }
        public Source Source { get; }
        public override bool Active => base.Active && Source.Active;

        public int CompareTo(BuyPrice other)
        {
            return -1 * Price.CompareTo(other.Price);
        }

        public BuyPrice(
            int price,
            Source source,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            Price = price;
            Source = source;
        }
    }
}
