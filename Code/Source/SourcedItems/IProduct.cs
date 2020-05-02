using System;

namespace StardewValleyStonks
{
    public interface IProduct : IPrice, IComparable<IProduct>
    {
        public string Name { get; }
        public int UnitPrice { get; }
        public double OutputPerInput { get; }
        public int Quality { get; }
    }
}
