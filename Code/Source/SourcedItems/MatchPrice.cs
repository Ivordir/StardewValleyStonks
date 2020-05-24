using System;

namespace StardewValleyStonks
{
    public class MatchPrice : BasePrice, IComparable<BuyPrice>
    {
        public override int Price => _Source.Match ? Match.Price : _Price;
        public override Source Source => _Source;

        private readonly int _Price;
        private readonly MatchSource _Source;
        private readonly BasePrice Match;

        public MatchPrice(
            int price,
            MatchSource source,
            BasePrice match,
            ICondition[] conditions = null)
            : base(source, conditions)
        {
            _Price = price;
            _Source = source;
            Match = match;
        }
    }
}