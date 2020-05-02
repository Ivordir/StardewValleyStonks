using System;

namespace StardewValleyStonks
{
    public class BoughtSeeds : Selectable, IReplant, IComparable<IReplant>
    {
        public IPriceFinder<ISource, BuyPrice> Seed { get; }

        public BoughtSeeds(
            IPriceFinder<ISource, BuyPrice> seed,
            bool enabled = true,
            ICondition[] conditions = null)
            : base (enabled, conditions)
        {
            Seed = seed;
        }

        public double Price => Seed.Price;
        public double UnitPrice => Seed.UnitPrice;
        public double Seeds => Seed.Amount;

        public int CompareTo(IReplant other)
        {
            return UnitPrice.CompareTo(other.UnitPrice);
        }

        public override bool Active => base.Active && Seed.HasBestPair;
    }
}
