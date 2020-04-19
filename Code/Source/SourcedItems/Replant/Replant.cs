using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class Replant : ActiveItem, IReplant, IComparable<IReplant>
    {
        public IInput[] Inputs { get; }

        public Replant(
            IInput[] inputs,
            bool enabled = true,
            List<ICondition> conditions = null)
            : base(enabled, conditions)
        {
            Inputs = inputs;
        }

        public double Price => UnitPrice * Seeds;

        public double UnitPrice
        {
            get
            {
                double sum = 0;
                foreach (IInput input in Inputs)
                {
                    sum += input.Price;
                }
                return sum;
            }
        }

        public double Seeds
        {
            get
            {
                double min = 0;
                foreach (IInput input in Inputs)
                {
                    if (input.Output < min)
                    {
                        min = input.Output;
                    }
                }
                return min;
            }
        }

        public int CompareTo(IReplant other)
        {
            return UnitPrice.CompareTo(other.UnitPrice);
        }
    }
}
