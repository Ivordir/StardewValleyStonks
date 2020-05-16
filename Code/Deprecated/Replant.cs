﻿using System;
using System.Linq;

namespace StardewValleyStonks
{
    public class Replant : Selectable, IReplant, IComparable<IReplant>
    {
        public IInput[] Inputs { get; }

        public Replant(
            IInput[] inputs,
            bool enabled = true,
            ICondition[] conditions = null)
            : base(enabled, conditions)
        {
            Inputs = inputs;
        }

        public double Price => Seeds * UnitPrice;
        public double UnitPrice => Inputs.Sum((i) => i.UnitPrice);
        public double Seeds => Inputs.Min((i) => i.Output);

        public int CompareTo(IReplant other)
        {
            return UnitPrice.CompareTo(other.UnitPrice);
        }
    }
}