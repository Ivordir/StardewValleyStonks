using System;

namespace StardewValleyStonks
{
    public class MatchPrice : Price
    {
        public override int Value => _Source.Match ? Match.Value : _Value;
        public override Source Source => _Source;

        private readonly int _Value;
        private readonly MatchSource _Source;
        private readonly Price Match;

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