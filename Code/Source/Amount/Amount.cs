namespace StardewValleyStonks
{
    public class Amount : IAmount
    {
        public double Value { get; set; }

        public Amount(double value)
        {
            Value = value;
        }
    }
}
