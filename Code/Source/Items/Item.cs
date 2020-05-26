namespace StardewValleyStonks
{
    public class Item : IItem
    {
        public string Name { get; }
        public int Price => Multiplier != null && Multiplier.Active ? (int)(Multiplier.Value * BasePrice) : BasePrice;
        public IItem Normal => this;

        private readonly int BasePrice;
        private readonly IMultiplier Multiplier;

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
