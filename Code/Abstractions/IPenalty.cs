namespace StardewValleyStonks
{
    public interface IPenalty
    {
        public bool AddMode { get; set; }
        public double Value { get; set; }

        public double ApplyTo(int basePrice);
    }
}
