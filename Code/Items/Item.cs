namespace StardewValleyStonks
{
    //potentially should instead be a class
    public readonly struct Item : IItem
    {
        public static Item None = new Item();

        //eh.....
        public static bool operator ==(Item a, Item b)
        => a.Name == b.Name;
        public static bool operator !=(Item a, Item b)
        => a.Name != b.Name;

        public readonly string Name { get; }
        public readonly int Price => Multiplier != null && Multiplier.Active ? (int)(Multiplier.Value * BasePrice) : BasePrice;
        public readonly Item Normal => this;
        public readonly IItem With(int quality) => quality == 0 ? this : (IItem)new QualityItem(this, quality);
        public readonly int Quality => 0;

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
