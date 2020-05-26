namespace StardewValleyStonks
{
    public class Item : IItem
    {
        public string Name { get; }
        public int Price => Multiplier != null && Multiplier.Active ? (int)(Multiplier.Value * BasePrice) : BasePrice;
        public IItem Normal => this;
        public IItem this[int index] => Qualitites[index];

        private readonly int BasePrice;
        private readonly IMultiplier Multiplier;
        private readonly IItem[] Qualitites;

        public Item(
            string name,
            int basePrice,
            IMultiplier multiplier = null,
            int qualities = 3)
        {
            Name = name;
            BasePrice = basePrice;
            Multiplier = multiplier;
            Qualitites = new IItem[qualities];
            Qualitites[0] = this;
            for (int quality = 1; quality < qualities - 1; quality++)
            {
                Qualitites[quality] = new QualityItem(this, quality);
            }
        }
    }
}
