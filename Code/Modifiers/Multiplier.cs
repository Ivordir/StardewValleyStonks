namespace StardewValleyStonks
{
    public class Multiplier : IMultiplier
    {
        public string Name { get; }
        public double Value { get; }

        public Multiplier(
            string name,
            double value)
        {
            Name = name;
            Value = value;
        }
    }
}
