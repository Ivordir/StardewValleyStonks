using System;

namespace StardewValleyStonks
{
    public class Fertilizer : DataTableItem
    {
        public int Quality { get; }
        public float Speed { get; }
        public IBestItemTracker<ISource, IPrice> PriceManager { get; }

        public Fertilizer(string name, int quality, float speed, IBestItemTracker<ISource, IPrice> priceManager) : base(name)
        {
            Quality = quality;
            Speed = speed;
            PriceManager = priceManager;
        }

        public override bool Active { get => throw new NotImplementedException(); }

        public int Price => PriceManager.NoSource ? -1 : PriceManager.ItemFrom[PriceManager.BestSources[0]].Price;
    }
}
