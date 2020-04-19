using System;

namespace StardewValleyStonks
{
    public interface IReplant : IActiveItem, IComparable<IReplant>
    {
        public double Price { get; }
        public double UnitPrice { get; }
        public double Seeds { get; }
    }
}
