namespace StardewValleyStonks
{
    public struct RefValue : IValue
    {
        public double Value { get; set; }

        public static implicit operator RefValue(double value) => new RefValue(value);

        public RefValue(double value)
        {
            Value = value;
        }
    }
}
