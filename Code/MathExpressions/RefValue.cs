namespace StardewValleyStonks
{
    public class RefValue : IValue
    {
        public double Value { get; set; }

        public RefValue(double value)
        {
            Value = value;
        }
    }
}
