using System;

namespace StardewValleyStonks
{
    public class Price : Selectable, IComparable<Price>
    {
        public virtual int Value { get; }
        public Source Source { get; }
        public override bool Active => base.Active && Source.Active;
        public override string Warnings
        {
            get
            {
                string warnings = string.Empty;
                if (!base.Active)
                {
                    warnings += ($"Locally:" + base.Warnings).WrapTag("li");
                }
                if (!Source.Active)
                {
                    warnings += ($"Globally:" + Source.Warnings).WrapTag("li");
                }
                return $"{Source.Name}:" + warnings.WrapTag("ul");
            }
        }

        public int CompareTo(Price other) => -1 * Value.CompareTo(other.Value);

        public Price(
            int price,
            Source source,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            Value = price;
            Source = source;
        }
    }
}
