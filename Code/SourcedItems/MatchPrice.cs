namespace StardewValleyStonks
{
    public class MatchPrice : Price
    {
        public override int Value => _Source.Match ? Match.Value : _Value;
        public override Source Source => _Source;

        readonly int _Value;
        readonly MatchSource _Source;
        readonly Price Match;

        public MatchPrice(
            int price,
            MatchSource source,
            Price match,
            ICondition[] conditions = null)
            : base(source, conditions)
        {
            _Value = price;
            _Source = source;
            Match = match;
        }
    }
}