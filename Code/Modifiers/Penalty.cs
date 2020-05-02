using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class Penalty : Selectable, IPenalty
    {
        public bool AddMode { get; set; }
        public double Value { get; set; }

        public Penalty(double value, bool addMode = false, bool enabled = false, List<ICondition> conditions = null) : base(enabled, conditions)
        {
            Value = value;
            AddMode = addMode;
        }

        public double ApplyTo(int basePrice)
        {
            if (AddMode)
            {
                return basePrice - Value;
            }
            return Value * basePrice;
        }
    }
}
