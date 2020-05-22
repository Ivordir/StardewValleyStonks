using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class BuyPrice : Selectable, IComparable<BuyPrice>
    {
        public int Price { get; }
        public Source Source { get; }
        public override bool Active => base.Active && Source.Active;
        public override List<Warning> Warnings
        {
            get
            {
                _Warnings.Clear();
                if (!Active)
                {
                    NoSource.SubWarnings.Clear();
                    if (!base.Active)
                    {
                        NoSource.SubWarnings.AddRange(base.Warnings);
                    }
                    if (!Source.Active)
                    {
                        NoSource.SubWarnings.AddRange(Source.Warnings);
                    }
                    _Warnings.Add(NoSource);
                }
                return _Warnings;
            }
        }

        private readonly List<Warning> _Warnings;
        private readonly Warning NoSource;

        public int CompareTo(BuyPrice other)
        {
            return -1 * Price.CompareTo(other.Price);
        }

        public BuyPrice(
            int price,
            Source source,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            Price = price;
            Source = source;
            NoSource = new Warning($"{Source.Name}:");
            _Warnings = new List<Warning>();
        }
    }
}
