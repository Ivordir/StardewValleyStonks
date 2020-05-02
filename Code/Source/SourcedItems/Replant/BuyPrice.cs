﻿using System;

namespace StardewValleyStonks
{
    public class BuyPrice : Selectable, IPrice, IComparable<BuyPrice>
    {
        public double Price { get; }

        public BuyPrice(double price, bool enabled = true, ICondition[] conditions = null) : base(enabled, conditions)
        {
            Price = price;
        }

        public int CompareTo(BuyPrice other)
        {
            return -1 * Price.CompareTo(other.Price);
        }
    }
}
