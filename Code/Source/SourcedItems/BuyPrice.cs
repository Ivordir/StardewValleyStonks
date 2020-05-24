using System;

namespace StardewValleyStonks
{
    public class BuyPrice : BasePrice, IComparable<BuyPrice>
    {
        public override int Price { get; }
        public override Source Source { get; }

        public BuyPrice(
            int price,
            Source source,
            ICondition[] conditions = null)
            : base(source, conditions)
        {
            Price = price;
            Source = source;
        }
    }
}
