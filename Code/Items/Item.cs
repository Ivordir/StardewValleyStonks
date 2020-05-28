namespace StardewValleyStonks
{
    public readonly struct Item : IItem
    {
        public readonly string Name { get; }
        public readonly int Price => Multiplier != null && Multiplier.Active ? (int)(Multiplier.Value * BasePrice) : BasePrice;
        public readonly Item Normal => this;
        public readonly IItem[] WithQuality { get; }
        public readonly int Quality => 0;

        readonly int BasePrice;
        readonly IMultiplier Multiplier;

        public Item(
            string name,
            int basePrice,
            IMultiplier multiplier = null,
            int numQualities = 3)
        {
            Name = name;
            BasePrice = basePrice;
            Multiplier = multiplier;
            WithQuality = new IItem[numQualities];
            for (int quality = 1; quality < numQualities; quality++)
            {
                WithQuality[quality] = new QualityItem(this, quality);
            }
        }
    }
}
