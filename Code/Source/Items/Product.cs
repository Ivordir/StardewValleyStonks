namespace StardewValleyStonks
{
    public class Product : IItem
    {
        public string Name { get; }
        public int Price => Multiplier.Active ? (int)(Multiplier.Value * BasePrice) : BasePrice;
        public IItem Normal => this;

        private readonly int BasePrice;
        private readonly IMultiplier Multiplier;

        public Product(
            string name,
            int basePrice,
            IMultiplier multiplier)
        {
            Name = name;
            BasePrice = basePrice;
            Multiplier = multiplier;
        }
    }
}
