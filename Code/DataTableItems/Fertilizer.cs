using System;

namespace StardewValleyStonks
{
    public class Fertilizer : DataTableItem
    {
        public int Quality { get; }
        public double Speed { get; }

        public override bool Active { get => throw new NotImplementedException(); }

        public Fertilizer(
            string name,
            int quality,
            double speed,
            BestFinder<Source, BuyPrice> priceManager)
            : base(name, priceManager)
        {
            Quality = quality;
            Speed = speed;
        }
    }
}
