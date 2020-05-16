namespace StardewValleyStonks
{
    public class PriceMultiplier : IPriceMultiplier
    {
        public PriceMultiplier(string name, double value)
        {
            Name = name;
            Value = value;
        }
        public string Name { get; }
        public double Value { get; }

        public int ApplyTo(int basePrice) => (int)(Value * basePrice);
    }
}
