using System;

namespace StardewValleyStonks
{
    public class Replant : ActiveItem, IReplant, IComparable<IReplant>
    {
        IInput Input { get; }

        public Replant(
            IInput input,
            bool enabled = true,
            ICondition[] conditions = null)
            : base(enabled, conditions)
        {
            Input = input;
        }

        public double Price => Input.Price;
        public double UnitPrice => Input.UnitPrice;
        public double Seeds => Input.Output;

        public int CompareTo(IReplant other)
        {
            return UnitPrice.CompareTo(other.UnitPrice);
        }
    }
}
