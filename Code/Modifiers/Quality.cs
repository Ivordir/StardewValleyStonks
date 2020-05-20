namespace StardewValleyStonks
{
    public class Quality
    {
        public string Name { get; }
        public double Value { get; }

        public int ApplyTo(int basePrice) => (int)(Value * basePrice);

        public Quality(
            string name,
            double value)
        {
            Name = name;
            Value = value;
        }
    }
}
