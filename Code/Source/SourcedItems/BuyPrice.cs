namespace StardewValleyStonks
{
    public class BuyPrice : Price
    {
        public override int Value { get; }
        public override Source Source { get; }

        public BuyPrice(
            int price,
            Source source,
            ICondition[] conditions = null)
            : base(source, conditions)
        {
            Value = price;
            Source = source;
        }
    }
}
