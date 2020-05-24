﻿using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    public abstract class BasePrice : Selectable, IComparable<BuyPrice>
    {
        public abstract int Price { get; }
        public abstract Source Source { get; }
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

        public int CompareTo(BuyPrice other) => -1 * Price.CompareTo(other.Price);

        private readonly List<Warning> _Warnings;
        private readonly Warning NoSource;

        public BasePrice(
            Source source,
            ICondition[] conditions = null)
            : base(true, conditions)
        {
            NoSource = new Warning($"{source.Name}:");
            _Warnings = new List<Warning>();
        }
    }
}
