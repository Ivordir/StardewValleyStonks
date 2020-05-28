namespace StardewValleyStonks
{
    public class MatchPrice : Price
    {
        public override int Value => ((MatchSource)Source).Match ? Match.Value : base.Value;

        readonly Price Match;

        public MatchPrice(
            int price,
            MatchSource source,
            Price match,
            ICondition[] conditions = null)
            : base(price, source, conditions)
        {
            Match = match;
        }
    }
}