using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class BoughtItem : BaseItem<BoughtItem>
    {
        public BoughtItem(int price, bool enabled, List<ICondition> conditions = null) : base(price, enabled, conditions) { }

        public override bool IsBetterThan(BoughtItem other) => Price < other.Price;
    }
}
