namespace StardewValleyStonks
{
    public struct RefValue : IValue
    {
        public static explicit operator RefValue(double value) => new RefValue(value);
        public static implicit operator double(RefValue value) => value.Value;
        public static RefValue operator -(RefValue value, double amount)
        {
            value.Value -= amount;
            return value;
        }

        public double Value { get; set; }

        public RefValue(double value)
        {
            Value = value;
        }
    }
}
