namespace StardewValleyStonks
{
    public class Item
    {
        public string Name { get; }
        public int Price => Multiplier != null && Multiplier.Active ? (int)(Multiplier.Value * BasePrice) : BasePrice;
        public QualityItem With(int quality) => new QualityItem(this, quality);
        public QualityItem Normal => new QualityItem(this, 0);

        readonly int BasePrice;
        readonly IMultiplier Multiplier;

        public Item(
            string name,
            int basePrice,
            IMultiplier multiplier = null)
        {
            Name = name;
            BasePrice = basePrice;
            Multiplier = multiplier;
        }
    }
}
