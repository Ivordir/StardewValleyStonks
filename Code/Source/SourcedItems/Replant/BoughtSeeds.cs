using System;

namespace StardewValleyStonks
{
    public class BoughtSeeds : ActiveItem, IReplant, IComparable<IReplant>
    {
        public IPriceTracker<ISource, IPricedItem> Seed { get; }

        public BoughtSeeds(
            IPriceTracker<ISource, IPricedItem> seed,
            bool enabled = true,
            ICondition[] conditions = null)
            : base (enabled, conditions)
        {
            Seed = seed;
        }

        public double Price => Seed.Amount * Seed.UnitPrice;
        public double UnitPrice => Seed.UnitPrice;
        public double Seeds => Seed.Amount;

        public int CompareTo(IReplant other)
        {
            return UnitPrice.CompareTo(other.UnitPrice);
        }

        public override bool Active => base.Active && Seed.HasSource;
    }
}
