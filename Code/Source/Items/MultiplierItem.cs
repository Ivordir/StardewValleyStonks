namespace StardewValleyStonks
{
    public class MultiplierItem : IItem
    {
        public string Name { get; }
        public int Price => Multiplier.Active ? (int)(Multiplier.Value * BasePrice) : BasePrice;

        private readonly int BasePrice;
        private readonly IMultiplier Multiplier;

        public MultiplierItem(
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
