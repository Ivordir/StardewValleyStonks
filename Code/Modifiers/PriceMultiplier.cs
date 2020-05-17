namespace StardewValleyStonks
{
    public class PriceMultiplier : IPriceMultiplier
    {
        public string Name { get; }
        public double Value { get; }

        public PriceMultiplier(string name, double value)
        {
            Name = name;
            Value = value;
        }

        public int ApplyTo(int basePrice) => (int)(Value * basePrice);
    }
}
