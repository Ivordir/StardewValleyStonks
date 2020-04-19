using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class Multiplier : ActiveItem, IMultiplier
    {
        private readonly double Value;

        public Multiplier(
            double multiplier,
            bool enabled = false,
            List<ICondition> conditions = null)
            : base(enabled, conditions)
        {
            Value = multiplier;
        }

        public int ApplyTo(int basePrice) => Active ? (int)(Value * basePrice) : basePrice; 
    }
}
